(ns ScriptSchedule.test.core
  (:use [ScriptSchedule.core])
  (:use [midje.sweet])
  (:use [clojure.java.io])
  (:use [clojure.xml]))

(def expected-header "Set\tCharacter\tDialogs\tScene\tPage\n")

(def simple-script
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>
  <FinalDraft DocumentType=\"Script\" Template=\"No\" Version=\"1\">
    <Content>
      <Paragraph Number=\"3\" Type=\"Scene Heading\">
        <SceneProperties Page=\"1\"/>
        <Text>INT. FRONT DOOR - DAY</Text>
      </Paragraph>
      <gunk/>
      <Paragraph Number=\"4\" Type=\"Scene Heading\">
        <SceneProperties Page=\"2\"/>
        <Text>INT. GS - DAY</Text>
      </Paragraph>
      <Paragraph Type=\"Action\">
        <Text>some action</Text>
      </Paragraph>
      <Paragraph Type=\"Character\">
        <Text>UNCLE BOB</Text>
      </Paragraph>
      <Paragraph Type=\"Dialogue\">
        <Text>some dialog</Text>
      </Paragraph>
      <Paragraph Type=\"Action\">
        <Text>another action</Text>
      </Paragraph>
      <Paragraph Type=\"Character\">
        <Text>UNCLE BOB (</Text>
        <Text AdornmentStyle=\"-1\">CONTÕD</Text>
        <Text>)</Text>
      </Paragraph>
      <Paragraph Type=\"Dialogue\">
        <Text>more dialog</Text>
      </Paragraph>
    </Content>
  </FinalDraft>
  ")

(def simple-scene {
  :set "GS"
  :character "UNCLE BOB"
  :dialogs "2"
  :scene "4"
  :page "2"})


(fact (to-scene-line simple-scene) => "GS\tUNCLE BOB\t2\t4\t2")

(defn parsed-finalDraft [content]
  {:tag :FinalDraft,
   :attrs {:DocumentType "Script",
           :Template "No",
           :Version "1"},
   :content [{:tag :Content,
              :attrs nil,
              :content content}]})

(defn parsed-scene [set scene page]
  {:tag :Paragraph,
   :attrs {:Number scene,
           :Type "Scene Heading"},
   :content [{:tag :SceneProperties,
              :attrs {:Page page},
              :content nil}
             {:tag :Text,
              :attrs nil,
              :content [(str "INT. " set " - DAY")]}]})

(defn parsed-tag [tag]
  {:tag tag, :attrs nil, :content nil})

(defn parsed-action [action]
  {:tag :Paragraph,
   :attrs {:Type "Action"},
   :content [{:tag :Text,
              :attrs nil,
              :content [action]}]})
(defn parsed-actor [character]
  {:tag :Paragraph,
   :attrs {:Type "Character"},
   :content [{:tag :Text,
              :attrs nil,
              :content [character]}]})

(defn parsed-continued-actor [character]
  {:tag :Paragraph,
   :attrs {:Type "Character"},
   :content [{:tag :Text,
              :attrs nil,
              :content [(str character " (")]}
             {:tag :Text,
              :attrs {:AdornmentStyle "-1"},
              :content ["CONTÕD"]}
             {:tag :Text,
              :attrs nil,
              :content [")"]}]})

(defn parsed-dialog [dialog]
  {:tag :Paragraph,
   :attrs {:Type "Dialogue"},
   :content [{:tag :Text,
              :attrs nil,
              :content [dialog]}]})

(def parsed-script
  (parsed-finalDraft
    [(parsed-scene "FRONT DOOR" "3" "1")
     (parsed-tag :gunk)
     (parsed-scene "GS", "4" "2")
     (parsed-action "some action")
     (parsed-actor "UNCLE BOB")
     (parsed-dialog "some dialog")
     (parsed-action "another action")
     (parsed-continued-actor "UNCLE BOB")
     (parsed-dialog "more dialog")]))

(facts "low level unit tests"
  (fact "an empty script has no paragraphs"
    (paragraphs-from-script (parsed-finalDraft [])) => [])

  (fact "a script with no scene headings, or characters has no paragraphs"
    (paragraphs-from-script (parsed-finalDraft [(parsed-tag :gunk)
                                                (parsed-action "action")
                                                (parsed-dialog "dialog")])) => [])

  (fact "a script with one scene heading gives one scene paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene "GS" 3 2)])) => [(new-scene-head "GS" 3 2)])

  (fact "a script with two scene headings gives two scene paragraphs"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene "GS" 1 1)
         (parsed-scene "FRONT DOOR" 2 1)])) => [(new-scene-head "GS" 1 1) (new-scene-head "FRONT DOOR" 2 1)])

  (fact "a script with one character gives a character paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-actor "UB")])) => [(new-actor "UB")])

  (fact "a script with scene headings and characters creates paragraphs"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene "Office" 1 1)
         (parsed-action "some action")
         (parsed-actor "Uncle Bob")
         (parsed-dialog "dialog")
         (parsed-scene "Desk" 2 1)
         (parsed-actor "Sherlock")])) => [(new-scene-head "Office" 1 1)
                                          (new-actor "Uncle Bob")
                                          (new-scene-head "Desk" 2 1)
                                          (new-actor "Sherlock")])

  (fact "a script with a continued actor reates an actor paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-continued-actor "Uncle Bob")])) => [(new-actor "Uncle Bob")])

  (fact "can detect a scene header"
    (scene-header? (parsed-scene "GS" 3 2)) => truthy)

  (fact "can extract location from set"
    (extract-location "Int. LOCATION - Day") => "LOCATION")

  (fact "can extract location from set without time"
    (extract-location "Int. LOCATION") => "LOCATION")
  )

(facts "high level unit tests"
  (fact "an empty script yeilds no scenes"
    (scenes-from-script (parsed-finalDraft [])) => [])

  (fact "a script with just one scene heading yeilds one scene"
    (scenes-from-script
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)])) => [(new-scene "WSL" "" 0 2 1)])

  (fact "a script with two scene headings yeilds two scenes"
    (scenes-from-script
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)
         (parsed-scene "WSR" 3 2)])) => [(new-scene "WSL" "" 0 2 1)
                                         (new-scene "WSR" "" 0 3 2)])

  (fact "a script with a scene head and an actor yeilds an acted scene"
    (scenes-from-script
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)
         (parsed-actor "UB")])) => [(new-scene "WSL" "UB" 1 2 1)])

  (fact "a script with more than one actor in a scene will count dialogs"
    (scenes-from-script
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)
         (parsed-actor "UB")
         (parsed-continued-actor "UB")])) => [(new-scene "WSL" "UB" 2 2 1)]))

(facts "acceptance tests"
  (fact (scene-lines "script.xml") => ["FRONT DOOR\t\t0\t3\t1"
                                       "GS\tUNCLE BOB\t2\t4\t2"])

  (fact (parse "script.xml") => parsed-script)

  (fact (-main "script.xml") => nil)

  (against-background
    (before :contents (spit "script.xml" simple-script))
    (after :contents (delete-file "script.xml")))
  )