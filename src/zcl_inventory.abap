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
             END OF Ty_item,

             tt_item TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

    METHODS load_from_db.      "----------LOAD DATA FROM DATABASE METHOD DEFINITION


    METHODS search_item        "----------SEARCH ITEM METHOD DEFINITION"
     IMPORTING
         i_item_id TYPE string
     RETURNING
        VALUE(rs_item) TYPE ty_item.


    METHODS add_item            "----------ADD ITEM METHOD DEFINITION
      IMPORTING
        i_item_id  TYPE string
        i_name     TYPE string
        i_quantity TYPE i
      RETURNING
        VALUE(rv_added) TYPE abap_bool.


    METHODS get_item_count
      RETURNING VALUE(rv_count) TYPE i.

    METHODS get_total_quantity
        RETURNING VALUE(rv_total) TYPE i.

    METHODS update_quantity    "----------UPDATE QUANTITY METHOD DEFINITION
    "Read item by id
    "update quantity
        IMPORTING
            i_item_id TYPE string
            i_new_quantity TYPE i.

    METHODS delete_item        "----------DELETE ITEM METHOD DEFINITION
        IMPORTING
        i_item_id TYPE string.

    METHODS filter_item_using_quantity  "----------FILTER ITEM METHOD DEFINITION
        IMPORTING
        i_quantity TYPE i
        RETURNING VALUE(rt_items) TYPE tt_item.

    METHODS save_to_db.
    METHODS constructor.

  PRIVATE SECTION.
  "Internal table to store inventory item
    DATA it_inventory TYPE SORTED TABLE OF ty_item
    WITH UNIQUE KEY item_id.


    DATA lo_repository TYPE REF TO zcl_inventory_repository.



ENDCLASS.





CLASS zcl_inventory IMPLEMENTATION.

    METHOD constructor.
        lo_repository = NEW zcl_inventory_repository( ).
    ENDMETHOD.


    METHOD load_from_db.      "------------LOAD DATA FROM DATABASE METHOD IMPLEMENTATION


        DATA lt_db_items TYPE zcl_inventory_repository=>tt_db_item.
        "Clear existing in-memory data
        CLEAR it_inventory.

        lt_db_items = lo_repository->load_inventory( ).   "--------db call

        it_inventory = VALUE #(
            FOR ls_db IN lt_db_items
            (
                item_id  = ls_db-item_id
                name     = ls_db-name
                quantity = ls_db-quantity
            )
        ).

    ENDMETHOD.

    METHOD search_item.        "------------SEARCH ITEM METHOD IMPLEMENTATION

        "define field-symbols
        FIELD-SYMBOLS <fs_item> TYPE ty_item.
        READ TABLE it_inventory ASSIGNING <fs_item>
        WITH KEY item_id = i_item_id.

        IF sy-subrc <> 0.
            ASSERT 1 = 0. " Item not found
        ENDIF.

        rs_item = <fs_item>.
    ENDMETHOD.


    METHOD add_item.

        ASSERT i_quantity >= 0.

        FIELD-SYMBOLS <fs_item> TYPE ty_item.

        READ TABLE it_inventory ASSIGNING <fs_item>
        WITH KEY item_id = i_item_id.

        IF sy-subrc = 0.
        "Item already exists
            rv_added = abap_false.
         RETURN.
        ENDIF.

        INSERT VALUE ty_item(
        item_id  = i_item_id
        name     = i_name
        quantity = i_quantity
        ) INTO TABLE it_inventory.

        rv_added = abap_true.

ENDMETHOD.


    METHOD get_item_count.
        rv_count = lines( it_inventory ).
    ENDMETHOD.

    METHOD get_total_quantity.

        rv_total = REDUCE I(
            INIT total = 0
            FOR ls_item IN it_inventory
            NEXT total += ls_item-quantity
        ).
    ENDMETHOD.

    METHOD update_quantity.       "------------UPDATE ITEM METHOD IMPLEMENTATION

        "input validation
        ASSERT i_new_quantity >= 0. "prevent negative stock

        "define field-symbols
        FIELD-SYMBOLS <fs_item> TYPE ty_item.

        READ TABLE it_inventory ASSIGNING <fs_item>
            WITH KEY item_id = i_item_id.

        if sy-subrc <> 0. "<> means not equal to
             "ITEM NOT FOUND
             RETURN.
        ENDIF.

        <fs_item>-quantity = i_new_quantity.


    ENDMETHOD.

    METHOD delete_item.          "------------DELETE ITEM METHOD IMPLEMENTATION

        DELETE it_inventory WHERE item_id = i_item_id.

        IF sy-subrc <> 0.
            ASSERT 1 = 0. " Item not found
        ENDIF.

    ENDMETHOD.

    METHOD filter_item_using_quantity. "-------------FILTER ITEM METHOD IMPLEMENTATION

        rt_items = VALUE tt_item(
    FOR ls_item IN it_inventory
    WHERE ( quantity > i_quantity )
    ( ls_item )
  ).
    ENDMETHOD.

    METHOD save_to_db.           "------------SAVE TO DATABASE METHOD IMPLEMENTATION

        "create a local table
        DATA lt_db_items TYPE zcl_inventory_repository=>tt_db_item.

        "Map business memory items to db-shaped items
        lt_db_items = VALUE #(
           FOR ls_item IN it_inventory
            (
            client   = sy-mandt
            item_id  = ls_item-item_id
            name     = ls_item-name
            quantity = ls_item-quantity
            )
          ).

        lo_repository->save_inventory( lt_db_items ).  "--------db call
    ENDMETHOD.

ENDCLASS.
