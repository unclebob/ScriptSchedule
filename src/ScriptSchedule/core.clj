(ns ScriptSchedule.core
  (:use [clojure.xml])
  (:use [clojure.pprint])
  (:gen-class ))

(def header "Set\tCharacter\tDialogs\tScene\tPage\tNotes")

(defrecord Scene [set number page])

(defn new-scene-head [set number page]
  (Scene. set number page))

(defrecord Actor [name])

(defn new-actor [name]
  (Actor. name))

(defrecord Action [action])

(defn new-action [action]
  (Action. action))

(defn find-tag [tag tags]
  (first (filter #(= tag (:tag %)) tags)))

(defn is-paragraph-of-type [tag type]
  (and
    (= (:tag tag) :Paragraph )
    (= (:Type (:attrs tag)) type)))

(defn scene-header? [tag]
  (is-paragraph-of-type tag "Scene Heading"))

(defn scene-actor? [tag]
  (is-paragraph-of-type tag "Character"))

(defn scene-action? [tag]
  (is-paragraph-of-type tag "Action"))

(defn extract-location [set]
  (if (nil? set)
    "NONE"
    (let [first-space (.indexOf set ". ")
          location-start (if (> 0 first-space) 0 (+ 2 first-space))
          dash (.indexOf set " - ")
          location-end (if (> 0 dash) (count set) dash)]
      (subs set location-start location-end))))

(defn get-first-text [content]
  (-> (find-tag :Text content) :content first))

(defn get-all-text [content]
  (let [text-tags (filter #(= :Text (:tag %)) content)
        texts (map #(first (:content %)) text-tags)]
    (apply str texts)))

(defn make-scene [tag]
  (let [attrs (:attrs tag)
        content (:content tag)
        page (-> (find-tag :SceneProperties content) :attrs :Page )
        set (get-first-text content)]
    (new-scene-head (extract-location set) (:Number attrs) page)))

(defn remove-cont [name]
  (if (= \( (last name))
    (subs name 0 (- (count name) 2))
    name))

(defn make-actor [tag]
  (let [content (:content tag)
        actor-name (get-first-text content)]
    (new-actor (remove-cont actor-name))))

(defn make-action [tag]
  (let [content (:content tag)
        action (get-all-text content)]
    (new-action action)))

(defn add-paragraphs [tags paragraphs]
  (if (empty? tags)
    paragraphs
    (let [tag (first tags)]
      (cond
        (scene-header? tag)
        (add-paragraphs (rest tags) (conj paragraphs (make-scene tag)))

        (scene-actor? tag)
        (add-paragraphs (rest tags) (conj paragraphs (make-actor tag)))

        (scene-action? tag)
        (add-paragraphs (rest tags) (conj paragraphs (make-action tag)))

        :else (add-paragraphs (rest tags) paragraphs)))))

(defn paragraphs-from-script [script]
  (let [tags (xml-seq script)]
    (add-paragraphs tags [])))

(defn new-scene
  ([set character dialogs number page]
    (new-scene set character dialogs number page ""))

  ([set character dialogs number page notes]
    (let [scene {:set set
                 :character character
                 :dialogs dialogs
                 :scene number
                 :page page
                 :notes notes
                 :title ""}]
      scene)))

(defn merge-scene [scenes scene]
  (if (nil? scene)
    scenes
    (conj scenes scene)))

(defn make-scene-from-head [head]
  (new-scene (.set head) "" 0 (.number head) (.page head)))

(defn add-actor-to-scene [actor scene]
  (assoc scene :character (.name actor) :dialogs (inc (:dialogs scene))))

(defn append-note [scene note]
  (assoc scene :notes (str (:notes scene) note)))

(defn add-note-to-scene [note scene]
  (let [trimmed-note (.trim (subs note 5))
        space-index (.indexOf trimmed-note " ")
        note-end (if (neg? space-index) (count trimmed-note) space-index)
        trimmed-note (subs trimmed-note 0 note-end)]
    (if (= (:notes scene) "")
      (append-note scene trimmed-note)
      (append-note scene (str "," trimmed-note)))))

(defn add-title-to-scene [title scene]
  (let [trimmed-title (.trim (subs title 6))]
    (assoc scene :title trimmed-title)))

(defn is-note [action-text]
  (.startsWith (.toLowerCase action-text) "note:"))

(defn is-title [action-text]
  (.startsWith (.toLowerCase action-text) "title:"))

(defn add-action-to-scene [action scene]
  (let [action-text (.action action)]
    (cond
      (nil? action-text) scene
      (is-note action-text) (add-note-to-scene action-text scene)
      (is-title action-text) (add-title-to-scene action-text scene)
      :default scene)))

(defn scenes-from-script
  ([script]
    (scenes-from-script (paragraphs-from-script script) [] nil))
  ([paragraphs scenes scene]
    (if (empty? paragraphs)
      (merge-scene scenes scene)
      (let [paragraph (first paragraphs)]
        (cond
          (= Scene (type paragraph))
          (scenes-from-script (rest paragraphs) (merge-scene scenes scene) (make-scene-from-head paragraph))

          (= Actor (type paragraph))
          (scenes-from-script (rest paragraphs) scenes (add-actor-to-scene paragraph scene))

          (= Action (type paragraph))
          (scenes-from-script (rest paragraphs) scenes (add-action-to-scene paragraph scene))

          :else (scenes-from-script (rest paragraphs) scenes scene))))))

(defn scenes [file]
  (scenes-from-script (parse file)))

(defn to-scene-line [scene]
  (.toUpperCase
    (str (:set scene) "\t"
      (:character scene) "\t"
      (:dialogs scene) "\t"
      (:scene scene) "\t"
      (:page scene) "\t"
      (:notes scene) "\t"
      (:title scene)
      )))

(defn scene-lines [file]
  (let [scenes (scenes file)]
    (map to-scene-line scenes)))

(defn -main [& args]
  (let [schedule (scene-lines (first args))]
    (println "Scenes: " (count schedule))
    (println header)
    (doseq [line schedule]
      (println line))))
