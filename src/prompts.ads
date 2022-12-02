package Prompts is
   type Answer is
     (No, --  Answer is "No"
      Yes, --  Answer is "Yes"
      Other,  --  "other" - useful for providing 3rd option
      DisableDefault);

   function Get_Answer
     (Prompt        : String; Default_Answer : Answer := DisableDefault;
      Provided_Text : String := "") return Answer;

   function Get_Answer
     (Prompt        : String; Default_Answer : String := "";
      Provided_Text : String := "") return String;
end Prompts;
