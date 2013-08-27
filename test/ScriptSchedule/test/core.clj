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
      <Paragraph Type=\"Action\">
        <Text>Title: This is a title</Text>
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
        <Text>Note: note another action</Text>
      </Paragraph>
      <Paragraph Type=\"Character\">
        <Text>UNCLE BOB (</Text>
        <Text AdornmentStyle=\"-1\">CONT�D</Text>
        <Text>)</Text>
      </Paragraph>
      <Paragraph Type=\"Dialogue\">
        <Text>more dialog</Text>
      </Paragraph>
      <Paragraph Number=\"5\" Type=\"Scene Heading\">
        <SceneProperties Page=\"3\"/>
        <Text>INT. FRONT DOOR - DAY</Text>
      </Paragraph>
      <Paragraph Type=\"Action\">
        <Text>Note:n1</Text>
      </Paragraph>
      <Paragraph Type=\"Action\">
        <Text>Note:n2</Text>
      </Paragraph>
      <Paragraph Type=\"Character\">
        <Text>UNCLE BOB</Text>
      </Paragraph>
      <Paragraph Type=\"Dialogue\">
        <Text>some dialog</Text>
      </Paragraph>
    </Content>
  </FinalDraft>
  ")

(def simple-scene
  (new-scene "GS" "UNCLE BOB" 2 4 2))


(fact (to-scene-line simple-scene) => "GS\tUNCLE BOB\t2\t4\t2\t\t")

(defn parsed-finalDraft [content]
  {:tag :FinalDraft,
   :attrs {:DocumentType "Script",
           :Template "No",
           :Version "1"},
   :content [{:tag :Content,
              :attrs nil,
              :content content}]})

(defn parsed-scene-raw [set scene page]
  {:tag :Paragraph,
   :attrs {:Number scene,
           :Type "Scene Heading"},
   :content [{:tag :SceneProperties,
              :attrs {:Page page},
              :content nil}
             {:tag :Text,
              :attrs nil,
              :content [set]}]})

(defn parsed-scene [set scene page]
  (parsed-scene-raw (str "INT. " set " - DAY") scene page))

(defn parsed-tag [tag]
  {:tag tag, :attrs nil, :content nil})

(defn parsed-action [action]
  {:tag :Paragraph,
   :attrs {:Type "Action"},
   :content [{:tag :Text,
              :attrs nil,
              :content [action]}]})

(defn parsed-double-action [action1 action2]
  {:tag :Paragraph,
   :attrs {:Type "Action"},
   :content [{:tag :Text,
              :attrs nil,
              :content [action1]}
             {:tag :Text,
              :attrs nil,
              :content [action2]}]})

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
              :content ["CONT�D"]}
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
     (parsed-action "Title: This is a title")
     (parsed-tag :gunk )
     (parsed-scene "GS", "4" "2")
     (parsed-action "some action")
     (parsed-actor "UNCLE BOB")
     (parsed-dialog "some dialog")
     (parsed-action "Note: note another action")
     (parsed-continued-actor "UNCLE BOB")
     (parsed-dialog "more dialog")
     (parsed-scene "FRONT DOOR" "5" "3")
     (parsed-action "Note:n1")
     (parsed-action "Note:n2")
     (parsed-actor "UNCLE BOB")
     (parsed-dialog "some dialog")]))


(facts "low level unit tests"
  (fact "an empty script has no paragraphs"
    (paragraphs-from-script (parsed-finalDraft [])) => [])

  (fact "a script with no scene headings, characters, or actions has no paragraphs"
    (paragraphs-from-script (parsed-finalDraft [(parsed-tag :gunk )
                                                (parsed-dialog "dialog")])) => [])

  (fact "a script with one scene heading gives one scene paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene "GS" 3 2)])) => [(new-scene-head "GS" 3 2)])

  (fact "a scene without a daytime is parsed correctly"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene-raw "INT. GS" 3 2)])) => [(new-scene-head "GS" 3 2)])

  (fact "a scene without a int/ext is parsed correctly"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene-raw "GS - DAY" 3 2)])) => [(new-scene-head "GS" 3 2)])

  (fact "a scene without a daytime or int/ext is parsed correctly"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene-raw "GS" 3 2)])) => [(new-scene-head "GS" 3 2)])

  (fact "a script with two scene headings gives two scene paragraphs"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene "GS" 1 1)
         (parsed-scene "FRONT DOOR" 2 1)])) => [(new-scene-head "GS" 1 1) (new-scene-head "FRONT DOOR" 2 1)])

  (fact "a script with one character gives a character paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-actor "UB")])) => [(new-actor "UB")])

  (fact "a script with one action gives an action paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-action "action")])) => [(new-action "action")])

  (fact "a script with one action having two text tags gives an action paragraph"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-double-action "action1" "-action2")])) => [(new-action "action1-action2")])

  (fact "a script with scene headings and characters creates paragraphs"
    (paragraphs-from-script
      (parsed-finalDraft
        [(parsed-scene "Office" 1 1)
         (parsed-action "some action")
         (parsed-actor "Uncle Bob")
         (parsed-dialog "dialog")
         (parsed-scene "Desk" 2 1)
         (parsed-actor "Sherlock")])) => [(new-scene-head "Office" 1 1)
                                          (new-action "some action")
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
    (build-scenes-from-script-xml (parsed-finalDraft [])) => [])

  (fact "a script with just one scene heading yeilds one scene"
    (build-scenes-from-script-xml
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)])) => [{:character "", :dialogs 0, :notes "",
                                          :page 1, :scene 2, :set "WSL", :title ""}])

  (fact "a script with two scene headings yeilds two scenes"
    (build-scenes-from-script-xml
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)
         (parsed-scene "WSR" 3 2)])) => [{:character "", :dialogs 0, :notes "",
                                          :page 1, :scene 2, :set "WSL", :title ""}
                                         {:character "", :dialogs 0, :notes "",
                                          :page 2, :scene 3, :set "WSR", :title ""}])

  (fact "a script with a scene head and an actor yeilds an acted scene"
    (build-scenes-from-script-xml
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)
         (parsed-actor "UB")])) => [{:character "UB", :dialogs 1, :notes "",
                                     :page 1, :scene 2, :set "WSL", :title ""}])

  (fact "a script with more than one actor in a scene will count dialogs"
    (build-scenes-from-script-xml
      (parsed-finalDraft
        [(parsed-scene "WSL" 2 1)
         (parsed-actor "UB")
         (parsed-continued-actor "UB")])) => [{:character "UB", :dialogs 2, :notes "",
                                               :page 1, :scene 2, :set "WSL", :title ""}]))

(fact "a script with a simple action in a scene has no effect"
  (build-scenes-from-script-xml
    (parsed-finalDraft
      [(parsed-scene "WSL" 2 1)
       (parsed-actor "UB")
       (parsed-action "some action")])) => [{:character "UB", :dialogs 1, :notes "",
                                             :page 1, :scene 2, :set "WSL", :title ""}])

(fact "a script with a simple note action in a scene adds the note"
  (build-scenes-from-script-xml
    (parsed-finalDraft
      [(parsed-scene "WSL" 2 1)
       (parsed-actor "UB")
       (parsed-action "Note: a")])) => [{:character "UB", :dialogs 1, :notes "a",
                                         :page 1, :scene 2, :set "WSL", :title ""}])

(fact "a script with a complex note action in a scene adds the note"
  (build-scenes-from-script-xml
    (parsed-finalDraft
      [(parsed-scene "WSL" 2 1)
       (parsed-actor "UB")
       (parsed-action "Note: a Some Note")])) => [{:character "UB", :dialogs 1, :notes "a",
                                                   :page 1, :scene 2, :set "WSL", :title ""}])

(fact "a script with several note actions in a scene adds the notes"
  (build-scenes-from-script-xml
    (parsed-finalDraft
      [(parsed-scene "WSL" 2 1)
       (parsed-actor "UB")
       (parsed-action "Note: a Some Note")
       (parsed-action "note: b some other note")])) => [{:character "UB", :dialogs 1, :notes "a,b",
                                                         :page 1, :scene 2, :set "WSL", :title ""}])

(fact "a script with a title action in a scene sets the title"
  (build-scenes-from-script-xml
    (parsed-finalDraft
      [(parsed-scene "WSL" 2 1)
       (parsed-actor "UB")
       (parsed-action "Title: some title")])) => [{:character "UB", :dialogs 1, :notes "",
                                                   :page 1, :scene 2, :set "WSL", :title "some title"}])

(facts "acceptance tests"
  (fact (to-scene-lines "script.xml") => ["FRONT DOOR\t\t0\t3\t1\t\tTHIS IS A TITLE"
                                       "GS\tUNCLE BOB\t2\t4\t2\tNOTE\t"
                                       "FRONT DOOR\tUNCLE BOB\t1\t5\t3\tN1,N2\t"])

  (fact (parse "script.xml") => parsed-script)

  (fact (-main "script.xml") => nil)

  (against-background
    (before :contents (spit "script.xml" simple-script))
    (after :contents (delete-file "script.xml")))
  )