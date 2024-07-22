(ns wire
  (:require [clojure.spec.alpha :as s])
  (:import [java.net UnixDomainSocketAddress]
           java.io.FileDescriptor
           java.nio.channels.ServerSocketChannel
           java.net.StandardProtocolFamily
           java.nio.channels.SocketChannel
           [java.nio ByteBuffer ByteOrder]
           org.newsclub.net.unix.FileDescriptorCast
           org.newsclub.net.unix.AFUNIXSocketChannel
           org.newsclub.net.unix.AFUNIXSocket))


(defn socket-channel [^String path]
  (let [^SocketChannel channel (AFUNIXSocketChannel/open StandardProtocolFamily/UNIX)]
    (.connect channel (UnixDomainSocketAddress/of path))
    channel))

(defn ->fd [i]
  (-> (org.newsclub.net.unix.FileDescriptorCast/unsafeUsing i)
      (.as java.io.FileDescriptor)))

(defn pack-string [^String s]
  (let [length  (if s (inc (.length s)) 0) ;; length includes the null terminator
        padding (mod (- 4 (mod length 4)) 4)
        total-length (+ length padding)
        buf-sz (+ 4 total-length)
        buf (-> (ByteBuffer/allocate buf-sz) (.order (ByteOrder/nativeOrder)))]
    (.putInt buf length)
    (.put buf (.getBytes s "UTF-8"))
    (.put buf (byte-array (inc padding))) ;; inc because of the null terminator
    buf))


(defn pack-arg [^AFUNIXSocketChannel sock type x]
  (let [^ByteBuffer bbuf
        (case type
          ;; Note: I haven't tested values outside of int range
          ("new_id" "uint" "object")
          (-> (ByteBuffer/allocate 4)
              (.order (ByteOrder/nativeOrder))
              (.putInt (unchecked-int x)))
          "int"
          (-> (ByteBuffer/allocate 4)
              (.order (ByteOrder/nativeOrder))
              (.putInt x))
          "string" (pack-string x)
          "fd"     (.setOutboundFileDescriptors sock (into-array FileDescriptor [(->fd x)])))]
    (some-> bbuf .flip)))

(defn pack-payload [sock args types]
  (into []
    (keep (fn [[t a]] (pack-arg sock t a)))
    (map vector types args)))

(defn write [^AFUNIXSocketChannel sock obj-id opcode args types]
  (let [bbufs (pack-payload sock args types)
        size (reduce + 8 (map (fn [b] (ByteBuffer/.capacity b)) bbufs))
        ^ByteBuffer
        header (-> (ByteBuffer/allocate 8)
                   (.order (ByteOrder/nativeOrder))
                   (.putInt obj-id)
                   (.putShort opcode)
                   (.putShort size)
                   .flip)]

    ;;(.write sock (into-array ByteBuffer (cons header bbufs)) 0 (inc (count bbufs)))
    ;;not sure why the array arities of .write isn't working for me
    (.write sock header)
    (doseq [^ByteBuffer b bbufs] (.write sock b))))

;; This assumes only a single fd per msg.
(defn parse-arg [^AFUNIXSocketChannel sock ^ByteBuffer bbuf type]
  (case type
    ("object" "uint")  (Integer/toUnsignedLong (.getInt bbuf))
    "int"  (.getInt bbuf)
    "array"
    (let [len     (Integer/toUnsignedLong (.getInt bbuf))
          padding (mod (- 4 (mod len 4)) 4)
          buf     (byte-array len)]
      (.get bbuf buf)
      (.position bbuf ^int (+ (.position bbuf) padding))
      buf)
    "string"
    (let [len     (Integer/toUnsignedLong (.getInt bbuf))
          ;; pad to 32 bits
          padding (mod (- 4 (mod len 4)) 4)
          buf     (-> len
                      dec ;;remove null terminator
                      byte-array)]

      (.get bbuf buf)
      (.position bbuf ^int (+ (.position bbuf)
                              1 ;; null terminator
                              padding))
      (String. buf))
    "fd" (first (.getReceivedFileDescriptors sock))))


(s/def ::oid int?)
(s/def ::opcode int?)
(s/def ::payload #(instance? ByteBuffer %))
(s/def ::wire-msg (s/keys :req-un [::oid ::opcode ::payload]))

(defn read-msg [^SocketChannel channel]
  (let [header (-> (ByteBuffer/allocate 8) (.order (ByteOrder/nativeOrder)))]
    (when (nat-int? (.read channel header))
      (.flip header)
      (let [object-id          (.getInt header)
            opcode             (.getShort header)
            size               (.getShort header)
            payload-length     (- size 8)
            payload            (-> (ByteBuffer/allocate payload-length)
                                   (.order (ByteOrder/nativeOrder)))
            payload-bytes-read (.read channel payload)]
        (when (nat-int? payload-bytes-read)
          (.flip payload)
          {:oid     object-id
           :opcode  opcode
           :payload payload})))))
