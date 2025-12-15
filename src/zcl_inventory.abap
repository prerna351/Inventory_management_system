CLASS zcl_inventory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Structure for one inventory item
        TYPES: BEGIN OF ty_item,
               item_id  TYPE string,
               name     TYPE string,
               quantity TYPE i,
             END OF Ty_item.

    METHODS:
    add_item
    "Methods to add inventory items
      IMPORTING
        i_item_id TYPE string
        i_name type string
        i_quantity type i.

    METHODS get_item_count
      RETURNING VALUE(rv_count) TYPE i.

    METHODS get_total_quantity
        RETURNING VALUE(rv_total) TYPE i.


  PRIVATE SECTION.
  "Internal table to store inventory item
    DATA it_inventory TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

ENDCLASS.



CLASS zcl_inventory IMPLEMENTATION.

    method add_item.

      "INPUT VALIDATION
       ASSERT i_quantity >= 0. " Prevent negative stock

      "Add inputs to the internal table
      APPEND VALUE ty_item(
        item_id = i_item_id
        name = i_name
        quantity = i_quantity
      ) to it_inventory.
    endmethod.

    METHOD get_item_count.
        rv_count = lines( it_inventory ).
    ENDMETHOD.

    METHOD get_total_quantity.
        rv_total = 0.

        LOOP AT it_inventory INTO DATA(ls_item).
            rv_total += ls_item-quantity.
        endLOOP.
    ENDMETHOD.

ENDCLASS.
