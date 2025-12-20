"THIS CLASS WILL ONLY TALK TO THE DATABASE

CLASS zcl_inventory_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
        TYPES:BEGIN OF ty_db_item,
              client TYPE mandt,
              item_id TYPE c LENGTH 30,
              name     TYPE c LENGTH 100,
              quantity TYPE i,
         END OF ty_db_item.
        TYPES tt_db_item TYPE STANDARD TABLE OF ty_db_item WITH EMPTY KEY.
        
        METHODS load_inventory          "-----LOAD INVENTORY METHOD---------------
            RETURNING VALUE(rt_items) TYPE tt_db_item.
    
        METHODS save_inventory          "-----SAVE INVENTORY METHOD--------------
            IMPORTING it_items TYPE tt_db_item.
        
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_inventory_repository IMPLEMENTATION.

    METHOD load_inventory.          "---------LOAD INVENTORY METHOD IMPLEMENTATION---------------
        SELECT  client,
                item_id,
                name,
                quantity
        FROM zinv_item
        INTO TABLE @rt_items.
    ENDMETHOD.
    
    
    
    
    METHOD save_inventory.          "----------SAVE INVENTORY METHOD IMPLEMENTATION--------------

         TRY.
             DELETE FROM zinv_item.             

             INSERT zinv_item FROM TABLE @it_items.

        COMMIT WORK.

        CATCH cx_root.
            ROLLBACK WORK.
        ENDTRY.

  ENDMETHOD.
  
ENDCLASS.
