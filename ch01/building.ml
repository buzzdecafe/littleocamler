#use "../common/base.ml";;

type seasoning = Salt | Pepper;;

type num = Zero | One_more_than of num;;

type 'a open_faced_sandwich = 
    Bread of 'a
  | Slice of 'a open_faced_sandwich;;



