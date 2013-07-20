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

(defn make-scene [tag]
  (let [attrs (:attrs tag)
        content (:content tag)
        page (-> (find-tag :SceneProperties content) :attrs :Page )
        set (-> (find-tag :Text content) :content first)]
    (new-scene-head (extract-location set) (:Number attrs) page)))

(defn remove-cont [name]
  (if (= \( (last name))
    (subs name 0 (- (count name) 2))
    name))

(defn make-actor [tag]
  (let [content (:content tag)
        actor-name (-> (find-tag :Text content) :content first)]
    (new-actor (remove-cont actor-name))))

(defn make-action [tag]
  (let [content (:content tag)
        action (-> (find-tag :Text content) :content first)]
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

(defn new-scene [set character dialogs number page actions]
  (let [scene {:set set
               :character character
               :dialogs dialogs
               :scene number
               :page page
               :actions actions}]
    scene))

(defn merge-scene [scenes scene]
  (if (nil? scene)
    scenes
    (conj scenes scene)))

(defn make-scene-from-head [head]
  (new-scene (.set head) "" 0 (.number head) (.page head) 0))

(defn add-actor-to-scene [actor scene]
  (assoc scene :character (.name actor) :dialogs (inc (:dialogs scene))))

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
          (scenes-from-script (rest paragraphs) scenes (assoc scene :actions (inc (:actions scene))))

          :else (scenes-from-script (rest paragraphs) scenes scene))))))

(defn scenes [file]
  (scenes-from-script (parse file)))

(defn to-scene-line [scene]
  (.toUpperCase
    (str (:set scene) "\t"
      (:character scene) "\t"
      (:dialogs scene) "\t"
      (:scene scene) "\t"
      (:page scene)
      (if (= 0 (:actions scene))
        ""
        (str "\t" (:actions scene)))
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
