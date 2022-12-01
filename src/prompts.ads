package Prompts is
   type Yes_or_No is (No, Yes, NA);

   function Get_Answer
     (Prompt : String; Default_Answer : Yes_or_No := NA) return Yes_or_No;

   function Get_Answer
     (Prompt : String; Default_Answer : String := "") return String;
end Prompts;
