;; Builds target/loregate.jar with nothing but Clojure and the JDK:
;;   clojure -M:uberjar
;; AOT-compiles only the launcher (loregate.launch), then copies every classpath jar and directory into one jar
;; (dropping jar signatures). Run: java -jar target/loregate.jar settings.edn
(ns uberjar
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)
           (java.util.jar JarEntry JarFile JarOutputStream Manifest Attributes$Name)))

(def classes "target/classes")
(def out "target/loregate.jar")

(defn- skip? [^String name]
  (or (re-matches #"(?i)META-INF/.*\.(SF|DSA|RSA|EC)" name)
      (= "META-INF/MANIFEST.MF" name)))

(defn -main [& _]
  (.mkdirs (io/file classes))
  (binding [*compile-path* classes] (compile 'loregate.launch))
  (let [cp (cons classes (str/split (System/getProperty "java.class.path") (re-pattern File/pathSeparator)))
        manifest (doto (Manifest.)
                   (-> .getMainAttributes (doto (.put Attributes$Name/MANIFEST_VERSION "1.0")
                                            (.put Attributes$Name/MAIN_CLASS "loregate.launch"))))
        seen (atom #{})]
    (io/make-parents out)
    (with-open [jar (JarOutputStream. (io/output-stream out) manifest)]
      ;; entries keep their original times: Clojure loads a namespace from source when its
      ;; .clj looks newer than its .class, and mixing the two breaks protocols and types
      (letfn [(put! [name time in-fn]
                (when-not (or (skip? name) (@seen name))
                  (swap! seen conj name)
                  (.putNextEntry jar (doto (JarEntry. ^String name) (.setTime time)))
                  (with-open [in (in-fn)] (io/copy in jar))
                  (.closeEntry jar)))]
        (doseq [entry cp
                :let [f (io/file entry)]
                :when (.exists f)]
          (if (.isDirectory f)
            (doseq [^File x (file-seq f) :when (.isFile x)]
              (put! (str/replace (subs (.getPath x) (inc (count (.getPath f)))) File/separator "/")
                    (.lastModified x) #(io/input-stream x)))
            (with-open [jf (JarFile. f)]
              (doseq [^JarEntry e (enumeration-seq (.entries jf)) :when (not (.isDirectory e))]
                (put! (.getName e) (.getTime e) #(.getInputStream jf e))))))))
    (println "wrote" out)))
