CLASS lhc_Z_R_CUSTOMER_TRAVEL_0631 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF customer_travel_status,
        open     TYPE c LENGTH 1 VALUE 'O',   " Open
        accepted TYPE c LENGTH 1 VALUE 'A',   " Accepted
        rejected TYPE c LENGTH 1 VALUE 'X',   " Rejected
      END OF customer_travel_status.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR CustomerTravel RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR CustomerTravel RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR CustomerTravel RESULT result.

    METHODS acceptClients FOR MODIFY
      IMPORTING keys FOR ACTION CustomerTravel~acceptClients RESULT result.

    METHODS rejectClients FOR MODIFY
      IMPORTING keys FOR ACTION CustomerTravel~rejectClients RESULT result.

    METHODS setDescription FOR DETERMINE ON SAVE
      IMPORTING keys FOR CustomerTravel~setDescription.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR CustomerTravel~validateCustomer.
    METHODS setStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR CustomerTravel~setStatus.
    METHODS updateFlightDate FOR MODIFY
      IMPORTING keys FOR ACTION CustomerTravel~updateFlightDate.

ENDCLASS.

CLASS lhc_Z_R_CUSTOMER_TRAVEL_0631 IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_root_entity)
    FAILED failed.

    result = VALUE #( FOR ls_root_entity IN lt_root_entity (
                                     %tky = ls_root_entity-%tky
                                     %action-acceptClients = COND #(
                                  WHEN ls_root_entity-OverallStatus = customer_travel_status-open
                                  THEN if_abap_behv=>fc-o-enabled
                                  ELSE if_abap_behv=>fc-o-disabled )
                                     %action-rejectClients = COND #(
                                  WHEN ls_root_entity-OverallStatus = customer_travel_status-open
                                  THEN if_abap_behv=>fc-o-enabled
                                  ELSE if_abap_behv=>fc-o-disabled ) ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

    DATA: lv_update_requested TYPE abap_bool,
          lv_update_granted   TYPE abap_bool,
          lv_delete_requested TYPE abap_bool,
          lv_delete_granted   TYPE abap_bool.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
         ENTITY CustomerTravel
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(lt_CustomerTravel)
         FAILED failed.

    lv_update_requested = COND #(
                              WHEN requested_authorizations-%update = if_abap_behv=>mk-on OR
                                   requested_authorizations-%action-Edit = if_abap_behv=>mk-on
                              THEN abap_true
                              ELSE abap_false ).
    lv_delete_requested = COND #(
                               WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                               THEN abap_true
                               ELSE abap_false ).

    CHECK lv_update_requested EQ abap_true.

    DATA(lv_technical_name) = cl_abap_context_info=>get_user_technical_name( ).

    LOOP AT lt_CustomerTravel INTO DATA(ls_CustomerTravel).

      IF lv_technical_name EQ 'CB998EEE141' AND ls_CustomerTravel-CurrencyCode EQ 'USD'.
        lv_update_granted = abap_false.

        APPEND VALUE #( %tky = ls_CustomerTravel-%tky
                        %msg = NEW /dmo/cm_flight_messages( textid = /dmo/cm_flight_messages=>not_authorized
                                                            severity = if_abap_behv_message=>severity-error )
                        %element-CurrencyCode = if_abap_behv=>mk-on ) TO reported-customertravel.

      ELSE.
        lv_update_granted = abap_true.
      ENDIF.

      IF lv_technical_name EQ 'CB998EEE141' AND ls_CustomerTravel-CurrencyCode EQ 'USD'.
        lv_delete_granted = abap_false.

        APPEND VALUE #( %tky = ls_CustomerTravel-%tky
                          %msg = NEW /dmo/cm_flight_messages( textid = /dmo/cm_flight_messages=>not_authorized
                                                              severity = if_abap_behv_message=>severity-error )
                          %element-CurrencyCode = if_abap_behv=>mk-on ) TO reported-customertravel.

      ELSE.
        lv_delete_granted = abap_true.
      ENDIF.

      APPEND VALUE #( LET upd_auth = COND #( WHEN lv_update_granted EQ abap_true
                                             THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                          del_auth = COND #( WHEN lv_delete_granted EQ abap_true
                                             THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                         IN
                         %tky = ls_CustomerTravel-%tky
                         %update = upd_auth
                         %action-Edit = upd_auth
                         %delete = del_auth ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_global_authorizations.

    DATA(lv_technical_name) = cl_abap_context_info=>get_user_technical_name( ).
*create
    IF requested_authorizations-%create EQ if_abap_behv=>mk-on.

      IF lv_technical_name EQ 'CB9980000872'.
        result-%create = if_abap_behv=>auth-allowed.
      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %msg = NEW /dmo/cm_flight_messages( textid    = /dmo/cm_flight_messages=>not_authorized
                                                             severity = if_abap_behv_message=>severity-error )
                       %global = if_abap_behv=>mk-on ) TO reported-customertravel.

      ENDIF.

    ENDIF.
*update
    IF requested_authorizations-%update EQ if_abap_behv=>mk-on
        OR requested_authorizations-%action-Edit EQ if_abap_behv=>mk-on.

      IF lv_technical_name EQ 'CB9980000872' .
        result-%update = if_abap_behv=>auth-allowed.
        result-%action-Edit = if_abap_behv=>auth-allowed.
      ELSE.
        result-%update = if_abap_behv=>auth-unauthorized.
        result-%action-Edit = if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %msg = NEW /dmo/cm_flight_messages( textid = /dmo/cm_flight_messages=>not_authorized
                                                             severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on  ) TO reported-customertravel.

      ENDIF.

    ENDIF.
*delete
    IF requested_authorizations-%delete EQ if_abap_behv=>mk-on.

      IF lv_technical_name EQ 'CB9980000872' .
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %msg = NEW /dmo/cm_flight_messages( textid = /dmo/cm_flight_messages=>not_authorized
                                                             severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on  ) TO reported-customertravel.

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD acceptClients.

    DATA: lt_updated_root_entity TYPE TABLE FOR UPDATE z_r_customer_travel_0631,
          lv_discount            TYPE /DMO/BT_DiscountPercentage,
          lv_disc_percent        TYPE f.

    DATA(keys_valid_discount) = keys.

    LOOP AT keys_valid_discount ASSIGNING FIELD-SYMBOL(<key_valid_discount>)
         WHERE %param-Travel_Discount IS INITIAL
         OR %param-Travel_Discount > 100
         OR %param-Travel_Discount <= 0.

      APPEND VALUE #( %tky = <key_valid_discount>-%tky ) TO  failed-customertravel.

      APPEND VALUE #( %tky = <key_valid_discount>-%tky
                      %msg = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>discount_invalid
                                                          severity = if_abap_behv_message=>severity-error )
                      %state_area = 'VALIDATE_COMPONENT'
                      %op-%action-acceptClients = if_abap_behv=>mk-on ) TO reported-customertravel.

      DATA(lv_error) = abap_true.

    ENDLOOP.

    CHECK lv_error NE abap_true.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_root_entity)
    FAILED failed.

    LOOP AT lt_root_entity ASSIGNING FIELD-SYMBOL(<ls_root_entity>).
      lv_discount = keys[ KEY id %tky = <ls_root_entity>-%tky ]-%param-Travel_Discount.
      lv_disc_percent = lv_discount / 100.
      <ls_root_entity>-Price = <ls_root_entity>-Price * ( 1 - lv_disc_percent ).
      <ls_root_entity>-OverallStatus = customer_travel_status-accepted.

      APPEND VALUE #( %tky = <ls_root_entity>-%tky
                      Price = <ls_root_entity>-Price
                      OverallStatus = <ls_root_entity>-OverallStatus ) TO lt_updated_root_entity.
    ENDLOOP.

    UNASSIGN <ls_root_entity>.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    UPDATE
    FIELDS ( Price
             OverallStatus )
    WITH lt_updated_root_entity.
    FREE lt_root_entity. " Free entries in lt_root_entity

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT lt_root_entity
    FAILED failed.

    RESUlt = VALUE #( FOR ls_Customer_Travel IN lt_root_entity ( %tky = ls_Customer_Travel-%tky
                                                               %param = ls_Customer_Travel ) ).

  ENDMETHOD.

  METHOD rejectClients.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
      ENTITY CustomerTravel
      UPDATE
      FIELDS ( OverallStatus )
      WITH VALUE #(  FOR ls_key IN keys ( %tky = ls_key-%tky
                                          OverallStatus = customer_travel_status-rejected )  ).

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_Customer_Travel)
    FAILED failed.

    RESUlt = VALUE #( FOR ls_Customer_Travel IN lt_Customer_Travel ( %tky   = ls_Customer_Travel-%tky
                                                                     %param = ls_Customer_Travel ) ).

  ENDMETHOD.

  METHOD setDescription.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(Travels).

    DELETE travels WHERE Description IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    UPDATE
    FIELDS ( Description )
    WITH VALUE #( FOR ls_travels IN travels ( %tky       = ls_travels-%tky
                                             Description = |Flight reason: { ls_travels-Description }| ) ).

  ENDMETHOD.

  METHOD validateCustomer.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    FIELDS ( CustomerId )
    WITH CORRESPONDING #( keys )
    RESULT DATA(Travels).

    DATA customers TYPE SORTED TABLE OF zcustomers_0631 WITH UNIQUE KEY client customer_uuid customer_id.

    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = CustomerId EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.

    IF customers IS NOT INITIAL.

      SELECT FROM zcustomers_0631 AS ddbb
             INNER JOIN @customers AS http_req ON http_req~customer_id EQ ddbb~customer_id
             FIELDS ddbb~customer_id
             INTO TABLE @DATA(valid_customers).

    ENDIF.

    LOOP AT travels INTO DATA(travel).

      IF travel-CustomerId IS INITIAL.

        APPEND VALUE #( %tky = travel-%tky ) TO failed-customertravel.

        APPEND VALUE #( %tky        = travel-%tky
                        %state_area = 'VALIDATE_COMPONENT'
                        %msg        = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>enter_customer_id
                                                            severity = if_abap_behv_message=>severity-error )
                        %element-CustomerId = if_abap_behv=>mk-on
                               ) TO reported-customertravel.

      ELSEIF travel-CustomerId IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerId ] ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-customertravel.

        APPEND VALUE #( %tky        = travel-%tky
                        %state_area = 'VALIDATE_COMPONENT'
                        %msg        = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>customer_unkown
                                                            severity = if_abap_behv_message=>severity-error )
                        %element-CustomerId = if_abap_behv=>mk-on
                               ) TO reported-customertravel.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD setStatus.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(Travels).

    DELETE travels WHERE OverallStatus IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    UPDATE
    FIELDS ( OverallStatus )
    WITH VALUE #( FOR ls_travels IN travels ( %tky         = ls_travels-%tky
                                             OverallStatus = customer_travel_status-open ) ).

* internal action
   MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
          ENTITY CustomerTravel
          EXECUTE updateFlightDate
         FROM CORRESPONDING #( KEYS ).

  ENDMETHOD.

  METHOD updateFlightDate.

    DATA lv_date TYPE d.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(Travels).

    DELETE travels WHERE FlightDate IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    lv_date = cl_abap_context_info=>get_system_date( ).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<fs_travels>).

      <fs_travels>-FlightDate = lv_date.

    ENDLOOP.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
           ENTITY CustomerTravel
           UPDATE FIELDS ( FlightDate )
           WITH CORRESPONDING #( travels ).

  ENDMETHOD.

ENDCLASS.
