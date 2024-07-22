(ns demo
  (:require [proto :as proto]
            [wire :as wire]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s])
  (:import
   java.nio.channels.SocketChannel
   com.sun.jna.platform.linux.LibC
   com.sun.jna.platform.linux.LibRT
   com.sun.jna.platform.linux.Fcntl
   com.sun.jna.platform.unix.LibCAPI
   com.sun.jna.platform.unix.LibCUtil
   com.sun.jna.platform.linux.Mman
   com.sun.jna.Native
   com.sun.jna.Pointer))

;; Just for reference:
(s/def ::interface string?)
(s/def ::oid int?)
(s/def ::op keyword?)
(s/def ::args sequential?)
(s/def ::obj-meta (s/keys :req [::proto/requests ::proto/events]))
(s/def ::obj (s/keys :req-un [::oid ::interface]))
(s/def ::req (s/keys :req-un [::obj ::op] :opt-un [::args]))


(defn new-ctx []
  (let [sock
        (wire/socket-channel
          (str (System/getenv "XDG_RUNTIME_DIR") "/" (System/getenv "WAYLAND_DISPLAY")))
        read-ch (a/chan 1000)
        req-ch (a/chan 100)
        this {::sock    sock
              ::read-ch read-ch
              ::req-ch req-ch
              ::state (atom {:prev-oid 0
                             :oid->obj {}})}]
    (future
      (loop []
        (when-let [msg (wire/read-msg sock)]
          (or (a/offer! read-ch msg)
              (println "WARN: read buffer full, dropping message msg"))
          (recur))))
    this))

(defn stop [{:keys [::sock] :as ctx}]
  (.close ^java.nio.channels.SocketChannel sock)
  (a/close! (::read-ch ctx))
  (a/close! (::req-ch ctx)))

(defn new-obj [ctx interface {:as handlers}]
  (let [{:keys [requests events] :as proto} (get proto/interface-spec interface)
        new-state (swap! (::state ctx)
                    (fn [{:keys [prev-oid] :as state}]
                      (let [oid (inc prev-oid)
                            obj (with-meta
                                  {:oid       oid
                                   :interface interface}
                                  {::proto/requests (update-keys requests keyword)
                                   ::proto/events events
                                   :event-opcode (->> events
                                                      (map (fn [[opcode {:keys [name]}]]
                                                             [(keyword name) opcode]))
                                                      (into {}))
                                   :handlers handlers})]
                        (-> (assoc state :prev-oid oid)
                            (update :oid->obj assoc oid obj)))))]
    (get-in new-state [:oid->obj (:prev-oid new-state)])))

(defn delete-obj [ctx oid]
  (swap! (::state ctx) update :oid->obj dissoc oid))

(defn -parse-msg [{:keys [::sock ::state] :as ctx} {:keys [oid opcode payload] :as msg}]
  (let [state  @(::state ctx)
        {:keys [interface] :as obj} (get-in state [:oid->obj (:oid msg)])]
    (if-let [{argspecs :args
              ename    :name}
             (get-in (meta obj) [::proto/events opcode])]
      {:op             (keyword ename)
       :args           (mapv (fn [a] (wire/parse-arg sock payload (:type a))) argspecs)
       :obj            obj
       :debug/argnames (mapv :name argspecs)}
      (println "WARN: no event found" interface opcode))))

(defn handle-msg [ctx msg]
  (let [event (-parse-msg ctx msg)
        handler (-> event :obj meta :handlers (get (:op event)))]
    (if handler
      (handler ctx event)
      #_(println "unhandled msg" event))))


(defn send-req [{:keys [::sock] :as ctx}
                {:keys                   [op obj args]
                 {:keys [oid interface]} :obj
                 :as                     req}]
  (if-let [{opcode :opcode argspecs :args} (-> obj meta ::proto/requests op)]
    (let [args (mapv (fn [x] (cond-> x (map? x) :oid)) args)] ;;maybe move this arg functionality to qreq
      {:wrote_bytes (wire/write sock oid opcode args (map :type argspecs))})
    (println "WARN: no request spec found for op" interface op)))


(defn destroy-obj [ctx {:keys [oid] :as obj}]
  (swap! (::state ctx) update :oid->obj dissoc oid)
  ctx)


(defn qreq "enqueues a request"
  [{:keys [::sock ::req-ch] :as ctx} req]
  (or
    (a/offer! req-ch req)
    (throw (ex-info "req-ch buffer full" {:request req}))))


(defn display-roundtrip!!
  "Blocks until all requests are processed by the server and it is finished sending events"
  [ctx display]
  (let [donep (a/chan 1)
        cb (new-obj ctx "wl_callback"
             {:done (fn [ctx cb]
                      (a/>!! donep true)
                      (destroy-obj ctx cb))})]
    (qreq ctx {:obj display :op :sync :args [cb]})
    (a/<!!
      (a/thread
        (loop [to (a/timeout 5000)]
          (a/alt!!
            to ([_] ::timeout)
            donep ([_] ::done)
            (::req-ch ctx) ([req] (send-req ctx req) (recur to))
            (::read-ch ctx) ([msg] (handle-msg ctx msg) (recur to))))))))

(defn display-dispatch [ctx]
  (let [{:keys [::req-ch ::read-ch ::sock]} ctx]
    (loop []
      (a/alt!!
        req-ch ([req]
                (when req
                  (send-req ctx req)
                  (recur)))
        read-ch ([msg]
                 (when msg
                   (handle-msg ctx msg)
                   (recur)))
        :priority true))))

(defn init [ctx]
  (let [globals-atom (atom {})
        wl-display   (new-obj ctx "wl_display"
                       {:delete_id (fn [ctx event]
                                     (delete-obj ctx (-> event :args first)))})
        global-handlers
        {"xdg_wm_base" {:ping (fn [ctx e]
                                (let [serial (-> e :args first)]
                                  (qreq ctx {:obj (:obj e) :op :pong :args [serial]})))}

         "wl_seat"     {:capabilities
                        (fn [ctx e]
                          (let [kb (new-obj ctx "wl_keyboard"
                                     {:key (fn [ctx e]
                                             #_(println "KEY" e)
                                             (let [[seriel time k state] (:args e)]
                                               (case (int k)
                                                 ;; "q" = 16
                                                 16 (stop ctx)
                                                 nil)))})]
                            (qreq ctx {:obj  (@globals-atom "wl_seat")
                                       :op   :get_keyboard
                                       :args [kb]})))
                        ;;:name (fn [ctx e] (println "seat name:" e))
                        }}
        wl-registry  (new-obj ctx "wl_registry"
                       {:global
                        (fn [ctx {:keys [obj args]}]
                          (let [[nom iface version] args]
                            (case iface
                              ("wl_compositor" "wl_shm" "xdg_wm_base" "wl_seat")
                              (let [o (new-obj ctx iface (get global-handlers iface))]
                                (qreq ctx {:obj obj :op :bind :args [nom iface version o]})
                                ;; todo: properly handle non-singletons
                                (swap! globals-atom assoc iface o))
                              nil
                              #_(println "unhandled global:" iface args))))})
        globals-prom (promise)]
    (qreq ctx {:obj wl-display :op :get_registry :args [wl-registry]})
    ;; since everything is processed in order, we know when we get
    ;; the "done" callback, every message we sent before has been
    ;; received/presumably processed. So `globals` should be ready after this

    ;; blocks until all sync'd up
    (display-roundtrip!! ctx wl-display) ;;todo: check for timeout/error
    @globals-atom))

(defn alc-shm [sz]
  (let [nom (subs (str (random-uuid)) 0 8)
        fd (.shm_open LibRT/INSTANCE
             nom
             (bit-or Fcntl/O_RDWR Fcntl/O_CREAT Fcntl/O_EXCL)
             (bit-or Fcntl/S_IWUSR Fcntl/S_IWOTH Fcntl/S_IROTH))]
    (.shm_unlink LibRT/INSTANCE nom)
    (LibCUtil/ftruncate fd sz)
    fd))

(defn new-obj-chs [ctx interface handler-chs]
  (let [handlers (into {}
                   (for [[k ch] handler-chs]
                     [k (fn [ctx event] (a/>!! ch event))]))]
    (new-obj ctx interface handlers)))

;;; Note: object's `oid`s are assigned when we call new-obj, but the server
;;; won't see the object until we call the appropriate "create" request with our
;;; local object included as a "new_id" arg, and the server receive all new_ids
;;; in order. It's easiest to keep track of if the "create" requests are called
;;; immediately after new-obj is called. I possibly should redesign things so
;;; that this isn't a concern.

(defn main-loop [ctx globals]
  (let [{:strs [wl_compositor wl_shm xdg_wm_base wl_seat]} globals
        frame-ch                                           (a/chan)
        xrfc-conf                                          (a/chan)
        top-conf                                           (a/chan)
        top-close                                          (a/chan)
        srfc                                               (new-obj ctx "wl_surface" {})
        _                                                  (qreq ctx {:obj wl_compositor :op :create_surface :args [srfc]})
        xrfc                                               (new-obj-chs ctx "xdg_surface" {:configure xrfc-conf})
        _                                                  (qreq ctx
                                                             {:obj  xdg_wm_base
                                                              :op   :get_xdg_surface
                                                              :args [xrfc srfc]})

        toplevel (new-obj-chs ctx "xdg_toplevel" {:configure top-conf
                                                  :close     top-close})
        _        (qreq ctx {:obj xrfc :op :get_toplevel :args [toplevel]})
        pixl     (atom nil)
        buf      (atom nil)
        color    (atom 0)
        w        (atom 200)
        h        (atom 200)
        draw!
        (fn []
          (.setMemory ^Pointer @pixl 0 (* @w @h) (unchecked-byte @color))
          (qreq ctx {:obj srfc :op :attach :args [@buf 0 0]})
          (qreq ctx {:obj srfc :op :damage :args [0 0 @w @h]})
          (qreq ctx {:obj srfc :op :commit :args []}))

        resz!
        (fn []
          (let [w @w h @h]
            (let [sz     (* w h 4)
                  fd     (alc-shm sz)
                  pool   (new-obj ctx "wl_shm_pool" {})
                  newbuf (new-obj ctx "wl_buffer" {})]
              (reset! pixl (LibCUtil/mmap nil sz (bit-or Mman/PROT_READ Mman/PROT_WRITE) Mman/MAP_SHARED fd 0))
              (reset! buf newbuf)
              (qreq ctx {:obj wl_shm :op :create_pool :args [pool fd sz]})
              ;;0 last arg is WL_SHM_FORMAT_ARGB8888
              (qreq ctx {:obj  pool :op :create_buffer
                         :args [newbuf 0 w h (* w 4) 0]})
              (qreq ctx {:obj pool :op :destroy :args []})
              ;;(.close LibC/INSTANCE fd) ;;saw this called in some c code but seems to break things
              )))]


    (a/thread
      (loop []
        (a/alt!!
          top-close ([_] (println "top closed"))
          xrfc-conf ([{[serial] :args :as event}]
                     (qreq ctx {:obj xrfc :op :ack_configure :args [serial]})
                     (when-not @pixl (resz!))

                     (draw!)
                     (recur))
          top-conf ([{[width height status] :args :as event}]
                    (when (and (pos? width) (pos? height))
                      (.munmap LibC/INSTANCE @pixl (com.sun.jna.platform.unix.LibCAPI$size_t. (* @w @h 4)))
                      (reset! w width)
                      (reset! h height)
                      (resz!))
                    (recur))
          frame-ch ([_]
                    (qreq ctx {:obj  srfc
                               :op   :frame
                               :args [(new-obj-chs ctx "wl_callback" {:done frame-ch})]})
                    (swap! color (fn [i] (mod (inc i) 255)))
                    (draw!)
                    (recur))
          :priority true))

      ;; cleanup:

      ;; todo: destroy keyboard
      (when-let [buf @buf]
        (qreq ctx {:obj buf :op :destroy}))
      (qreq ctx {:obj wl_seat :op :release})
      (qreq ctx {:obj toplevel :op :destroy})
      (qreq ctx {:obj xrfc :op :destroy}))


    (qreq ctx {:obj toplevel :op :set_title :args ["hello world"]})
    (qreq ctx {:obj srfc :op :commit})
    (qreq ctx {:obj srfc :op :frame :args [(new-obj-chs ctx "wl_callback" {:done frame-ch})]})

    (display-dispatch ctx)))


(defn start! [& {:as opts}]
  (let [ctx (new-ctx)
        globals (init ctx)]
    (main-loop ctx globals)))

(comment
  ;; blocks until window is exited
  (start!)
  )

(comment
  (def ctx1 (new-ctx))

  (def GLOBALS (init ctx1))
  (future (main-loop ctx1 GLOBALS))

  (stop ctx1)
  )
