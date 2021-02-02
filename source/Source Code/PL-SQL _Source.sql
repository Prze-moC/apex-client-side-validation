/*
  author: Bartosz Ostrowski ostrowski.bartosz@gmail.com, bostrowski@pretius.com
  1.0 release date: 2015-09-16
  1.1 release date: 2016-04-19
  1.2 release date: 2020-01-18 Przemek Czernek (czernek.p@dxc.com): added support for APEX 20.2 "EXPRESSION" type of conditions and validations
*/
-- DECLARE
e_validation_failed EXCEPTION;
TYPE t_tab IS TABLE OF VARCHAR2(4000) INDEX BY binary_integer;

FUNCTION render_validation (
  p_dynamic_action IN apex_plugin.t_dynamic_action,
  p_plugin         IN apex_plugin.t_plugin 
) RETURN apex_plugin.t_dynamic_action_render_result
IS
  v_result apex_plugin.t_dynamic_action_render_result;
BEGIN
  apex_css.add_file (  
    p_name => 'pretius_validation_style',
    p_directory => p_plugin.file_prefix, 
    p_version => NULL
  );

  apex_javascript.add_library(
    p_name      => 'pretius_validation',
    p_directory => p_plugin.file_prefix,
    p_version   => NULL 
  );

  v_result.ajax_identifier := APEX_PLUGIN.GET_AJAX_IDENTifIER();
  v_result.attribute_01 := p_dynamic_action.attribute_01;
  v_result.attribute_02 := REPLACE(REPLACE(p_dynamic_action.attribute_02, CHR(13), ''), CHR(10), '');
  v_result.attribute_04 := p_dynamic_action.attribute_04;
  v_result.attribute_05 := p_dynamic_action.attribute_05;
  v_result.attribute_06 := p_dynamic_action.attribute_06;
  v_result.attribute_07 := p_dynamic_action.attribute_07;
  v_result.attribute_08 := NVL(p_dynamic_action.attribute_08, 0);
  v_result.attribute_10 := p_plugin.attribute_01;

  v_result.javascript_function := 'function() { pretius_validation(this, '''||p_plugin.file_prefix||''');}';    

  APEX_PLUGIN_UTIL.DEBUG_DYNAMIC_ACTION (
    p_plugin         => p_plugin,
    p_dynamic_action => p_dynamic_action
  );  

  RETURN v_result;

EXCEPTION
  WHEN OTHERS THEN
    htp.p( SQLERRM );
    RETURN v_result;
END render_validation;

FUNCTION GET_VALUE (
  P_NAME IN VARCHAR2 )
  RETURN VARCHAR2
IS
  v_value VARCHAR2(32767);
BEGIN
  v_value := V(P_NAME);
  
  RETURN CASE
    WHEN v_value = '%null%' THEN NULL
    ELSE v_value
  END;
END GET_VALUE;

FUNCTION current_time_ms
  RETURN NUMBER
IS
  out_result NUMBER;
BEGIN
  SELECT 
    EXTRACT(DAY FROM(SYSTIMESTAMP - TO_TIMESTAMP('1970-01-01', 'YYYY-MM-DD'))) * 86400000 + TO_NUMBER(TO_CHAR(SYS_EXTRACT_UTC(SYSTIMESTAMP), 'SSSSSFF3'))
  INTO 
    out_result
  FROM 
    dual;
    
  RETURN out_result;
END current_time_ms;

FUNCTION perform_binds(
  p_string IN VARCHAR2,
  p_escape IN BOOLEAN DEFAULT FALSE
) RETURN VARCHAR2 
IS
  v_item_names DBMS_SQL.VARCHAR2_TABLE;
  
  v_item_value VARCHAR2(32767);
  v_string VARCHAR2(32767);
  v_test_number NUMBER;
  v_isnumber BOOLEAN := FALSE;
BEGIN
  v_string := p_string;
  v_item_names := WWV_FLOW_UTILITIES.GET_BINDS( v_string ); 
  
  FOR i IN 1..v_item_names.count LOOP
  
    v_item_value := APEX_UTIL.GET_SESSION_STATE (
      p_item => LTRIM(v_item_names(i), ':')
    );

    IF p_escape THEN
      --jesli w sesji jest uzyty apostrof, we-escape-uj go
      v_item_value := REPLACE(v_item_value, CHR(39), CHR(39)||CHR(39));
    END IF;

    IF v_item_value IS NULL THEN
      v_string := REPLACE(v_string, v_item_names(i), 'NULL');
      CONTINUE;
    END IF;

    BEGIN
      v_test_number := TO_NUMBER(v_item_value);  
      v_isnumber := TRUE;
    EXCEPTION
      WHEN others THEN
        v_isnumber := FALSE;
    END;

    IF v_isnumber THEN
      v_string := REPLACE(v_string, v_item_names(i), v_item_value);
    ELSE
      v_string := REPLACE(v_string, v_item_names(i), CHR(39)||v_item_value||CHR(39));
    END IF;
  
  END LOOP;
  
  v_string := apex_plugin_util.replace_substitutions (
    p_value => v_string,
    p_escape => TRUE 
  );
  
  RETURN v_string;
  
END perform_binds;

FUNCTION get_func_boolean_result(
  p_code IN VARCHAR2
) RETURN BOOLEAN
IS
  v_func_block VARCHAR2(32000);
  v_result VARCHAR2(1);
  v_code VARCHAR2(32000);
BEGIN
  
  v_code := perform_binds( p_code, TRUE );
  v_code := REPLACE(v_code, 'declare', 'DECLARE');
  v_code := REPLACE(v_code, 'begin', 'BEGIN');

  IF INSTR(v_code, 'DECLARE') > 0 THEN
    v_code := REPLACE(v_code, 'DECLARE', 'function test return boolean is');
  ELSIF INSTR(v_code, 'BEGIN') > 0 THEN
    v_code := REPLACE(v_code, 'BEGIN', 'function test return boolean is begin');
  ELSE
    v_code := 'function test return boolean is begin '||v_code||' end;';
  END IF;

  v_func_block := '
    declare
      v_result boolean;
      '||v_code||'
    begin
      v_result := test();

      if v_result then
        :out := 1;
      else
        :out := 0;
      end if;
    end; 
  ';
  --htp.p(v_func_block);
  execute IMMEDIATE v_func_block USING OUT v_result;

  IF v_result = 1 THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END IF;

END get_func_boolean_result;

FUNCTION get_plsql_expression_result(
  p_expression IN VARCHAR2
) RETURN BOOLEAN
IS
  v_expression VARCHAR2(32000);
  v_func_block VARCHAR2(32000);
  v_result VARCHAR2(1);
BEGIN
  v_expression := perform_binds( p_expression, TRUE );
  v_func_block := '
    begin 
      if '||v_expression||' then 
        return 1; 
      else 
       return 0; 
      end if;
    end;
  ';
 
  v_result := APEX_PLUGIN_UTIL.GET_PLSQL_FUNCTION_RESULT( v_func_block );

  IF v_result = '1' THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END IF;

END get_plsql_expression_result;

FUNCTION getItemFormatMask(
  p_item_id IN VARCHAR2
) RETURN VARCHAR2 IS
  v_format_mask VARCHAR2(200);
BEGIN
  SELECT 
    FORMAT_MASK 
  INTO
    v_format_mask
  FROM 
    APEX_APPLICATION_PAGE_ITEMS 
  WHERE 
    application_id = :APP_ID
    AND page_id = :APP_PAGE_ID 
    AND item_name = p_item_id;

  RETURN v_format_mask;  

EXCEPTION
  WHEN OTHERS THEN
    RETURN NULL;
END getItemFormatMask;

FUNCTION selectCountFromQuery(
  p_query IN VARCHAR2
) RETURN NUMBER
IS
  v_query VARCHAR2(32000);
  v_count NUMBER :=0;
BEGIN

  v_query := perform_binds( p_query, TRUE );
  v_query := 'select count(1) from ('|| v_query ||')';

  execute IMMEDIATE v_query INTO v_count;

  RETURN v_count;

END selectCountFromQuery;

FUNCTION rtrim_ws(
  p_val IN VARCHAR2
) RETURN VARCHAR2
AS

BEGIN
  RETURN RTRIM(REPLACE(REPLACE(p_val,CHR(13),NULL),CHR(10),NULL));
END rtrim_ws;

PROCEDURE isRunConditionMatched(
  p_condition_type_code IN VARCHAR2,
  p_condition_expression1 IN VARCHAR2,
  p_condition_expression2 IN VARCHAR2,
  p_out_run OUT NUMBER,
  p_out_msg OUT VARCHAR2

)
IS
  v_cond_type VARCHAR2(100) := p_condition_type_code;
  v_cond_1  VARCHAR2(32767) := WWV_FLOW.DO_SUBSTITUTIONS(p_condition_expression1);
  v_cond_2  VARCHAR2(32767) := WWV_FLOW.DO_SUBSTITUTIONS(p_condition_expression2);

  e_return_true         EXCEPTION;
  e_return_false        EXCEPTION;
  e_unkown_condition    EXCEPTION;
  e_return_no_condition EXCEPTION;

  v_test_number NUMBER;

BEGIN

  IF p_condition_type_code IS NULL THEN
    RAISE e_return_no_condition;
  END IF;

  CASE  v_cond_type
  WHEN 'NEVER'             THEN RAISE e_return_false;
  WHEN 'ALWAYS'            THEN RAISE e_return_true;
  WHEN 'SQL_EXPRESION'     THEN IF selectCountFromQuery( 'select 1 from dual where '||perform_binds( v_cond_1 ) ) = 0 THEN RAISE e_return_false; END IF;
  WHEN 'SQL_EXPRESSION'    THEN IF selectCountFromQuery( 'select 1 from dual where '||perform_binds( v_cond_1 ) ) = 0 THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_NULL'      THEN IF REPLACE(V(RTRIM_WS( v_cond_1 )),'%null%',NULL) IS NOT NULL THEN  RAISE e_return_false; END IF;
  WHEN 'FLOW_ITEM_IS_NULL' THEN IF REPLACE(V(RTRIM_WS( v_cond_1 )),'%null%',NULL) IS NOT NULL THEN  RAISE e_return_false; END IF;
  WHEN 'PLSQL_EXPRESSION'  THEN IF NOT get_plsql_expression_result( v_cond_1 ) THEN RAISE e_return_false; END IF; 
  WHEN 'PLSQL_EXPRESION'   THEN IF NOT get_plsql_expression_result( v_cond_1 ) THEN RAISE e_return_false; END IF; 
  WHEN 'FUNCTION_BODY'     THEN IF NOT get_func_boolean_result( v_cond_1 )     THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_ZERO'      THEN IF NVL(V(RTRIM_WS( v_cond_1 )),'x') <> '0'     THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_NOT_NULL'  THEN IF GET_VALUE( v_cond_1 ) IS NULL               THEN RAISE e_return_false; END IF;
  WHEN 'EXISTS'            THEN IF selectCountFromQuery( v_cond_1 ) <= 0       THEN RAISE e_return_false; END IF;
  WHEN 'NOT_EXISTS'        THEN IF selectCountFromQuery( v_cond_1 ) > 0        THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_NOT_ZERO'  THEN IF NVL( V( RTRIM_WS(v_cond_1) ),'x' ) = '0'    THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_NOT_ZERO'     THEN IF NVL( V( RTRIM_WS(v_cond_1) ),'x' ) = '0'    THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_NULL_OR_ZERO'          THEN IF V(RTRIM_WS( v_cond_1 )) IS NOT NULL AND V(RTRIM_WS( v_cond_1 )) != '0' THEN      RAISE e_return_false; END IF;
  WHEN 'VAL_OF_ITEM_IN_COND_EQ_COND2'  THEN IF GET_VALUE( v_cond_1 ) <> v_cond_2 OR GET_VALUE( v_cond_1 ) IS NULL THEN RAISE e_return_false;     END IF;       
  WHEN 'ITEM_CONTAINS_NO_SPACES'       THEN IF INSTR(REPLACE(V(WWV_FLOW.DO_SUBSTITUTIONS(RTRIM_WS(v_cond_1),'TEXT')),'%null%',NULL),' ') > 0 THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_NOT_NULL_OR_ZERO'         THEN IF V(RTRIM_WS(v_cond_1)) IS NULL OR V(RTRIM_WS(v_cond_1)) = '0' THEN RAISE e_return_false; END IF;    
  WHEN 'ITEM_IS_NOT_NULL_OR_ZERO'      THEN IF V(RTRIM_WS(v_cond_1)) IS NULL OR V(RTRIM_WS(v_cond_1)) = '0' THEN RAISE e_return_false; END IF;    
  WHEN 'FLOW_ITEM_IS_NOT_NULL_OR_ZERO' THEN IF V(RTRIM_WS(v_cond_1)) IS NULL OR V(RTRIM_WS(v_cond_1)) = '0' THEN RAISE e_return_false; END IF;    
  WHEN 'ITEM_IS_ALPHANUMERIC'          THEN IF RTRIM(UPPER(REPLACE(V(UPPER(v_cond_1)),'%null%',NULL)), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_') IS NOT NULL THEN RAISE e_return_false; END IF;
       /*  l_val VARCHAR2(4000);
           l_val := REPLACE(V(UPPER(v_cond_1)),'%null%',NULL);
           FOR J IN 1..NVL(LENGTH(REPLACE(V(WWV_FLOW.DO_SUBSTITUTIONS(UPPER(RTRIM_WS(v_cond_1)),'TEXT')),'%null%',NULL)),0) LOOP
             IF INSTR('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_',SUBSTR(L_VAL,J,1)) = 0 THEN
               RAISE e_return_false;
             END IF;
           END LOOP; */
  WHEN 'ITEM_IS_NUMERIC' THEN
       BEGIN
         v_test_number := REPLACE(V(WWV_FLOW.DO_SUBSTITUTIONS(RTRIM_WS(v_cond_1),'TEXT')),'%null%',NULL);
       EXCEPTION 
         WHEN others THEN RAISE e_return_false;
       END;
  WHEN 'ITEM_IS_NOT_NUMERIC' THEN
       BEGIN
          v_test_number := REPLACE(V(WWV_FLOW.DO_SUBSTITUTIONS(RTRIM_WS( v_cond_1 ),'TEXT')),'%null%',NULL);
          RAISE e_return_false; 
       EXCEPTION WHEN others THEN
          NULL;
       END;
  WHEN 'VAL_OF_ITEM_IN_COND_NOT_EQ_COND2'                       THEN IF NVL(V(RTRIM_WS( v_cond_1 )),'Mjhakb') = NVL(WWV_FLOW.DO_SUBSTITUTIONS( v_cond_1 ),'mjHbka')       THEN RAISE e_return_false; END IF;
  WHEN 'VAL_OF_ITEM_IN_COND_NOT_EQ_COND_TEXT'                   THEN IF NVL(V(RTRIM_WS( v_cond_1 )),'Mjhakb') = NVL(WWV_FLOW.DO_SUBSTITUTIONS( v_cond_1 ),'mjHbka')       THEN RAISE e_return_false; END IF;
  WHEN 'VALUE_OF_ITEM_NAMED_IN_COND1_NOT_EQUAL_TEXT_IN_COND2'   THEN IF NVL(V(RTRIM_WS( v_cond_1 )),'Mjhakb') = NVL(WWV_FLOW.DO_SUBSTITUTIONS( v_cond_1 ),'mjHbka')       THEN RAISE e_return_false; END IF;
  WHEN 'VALUE_OF_ITEM_IN_CONDITION_IN_COLON_DELIMITED_LIST'     THEN IF INSTR(':'||WWV_FLOW.DO_SUBSTITUTIONS(v_cond_2,'TEXT')||':',':'||V(RTRIM_WS( v_cond_1 ))||':') = 0 THEN RAISE e_return_false; END IF;
  WHEN 'VALUE_OF_ITEM_IN_CONDITION_NOT_IN_COLON_DELIMITED_LIST' THEN IF INSTR(':'||WWV_FLOW.DO_SUBSTITUTIONS(v_cond_2,'TEXT')||':',':'||V(RTRIM_WS( v_cond_1 ))||':') > 0 THEN RAISE e_return_false; END IF;
  WHEN 'EXPRESSION' THEN
       CASE v_cond_2
           WHEN 'SQL'   THEN IF selectCountFromQuery( 'select 1 from dual where '||perform_binds( v_cond_1 ) ) = 0 THEN RAISE e_return_false; END IF;
           WHEN 'PLSQL' THEN IF NOT get_plsql_expression_result( v_cond_1 ) THEN RAISE e_return_false; END IF;
           ELSE RAISE e_unkown_condition;
       END CASE;
  ELSE    RAISE e_unkown_condition;
  END CASE;

  RAISE e_return_true;

EXCEPTION
  WHEN e_return_no_condition THEN
    p_out_run := 1;
    p_out_msg := 'No condition for validation';
  WHEN e_return_true THEN
    p_out_run := 1;
    p_out_msg := 'Condition ['||v_cond_type||'] passed.';
  WHEN e_return_false THEN
    p_out_run := 0;
    p_out_msg := 'Condition ['||v_cond_type||'] not passed. Validation is not executed.';
  WHEN e_unkown_condition THEN
    p_out_run := 2;
    p_out_msg := 'Condition type ['|| v_cond_type || ']  not supported. Sorry';
    APEX_DEBUG.ERROR ( 'Condition type [%s] ("%s", "%s") not supported.', v_cond_type, v_cond_1, v_cond_2);  
  WHEN OTHERS THEN
    p_out_run := 2;
    p_out_msg := 'Unexpected error: '||SQLERRM;
    APEX_DEBUG.ERROR ( 'Unexpected error: '||SQLERRM);  
END isRunConditionMatched;

PROCEDURE VALIDATE(
  P_VALIDATION_TYPE IN VARCHAR2,
  p_validation_name IN VARCHAR2,
  p_validation_expression1 IN VARCHAR2,
  p_validation_expression2 IN VARCHAR2,
  p_validation_error_text IN VARCHAR2,
  p_out_error_text OUT VARCHAR2,
  p_out_status OUT NUMBER,
  p_out_msg OUT VARCHAR2
)
IS
  L_EXPRESSION1 VARCHAR2(32767) := WWV_FLOW.DO_SUBSTITUTIONS(p_validation_expression1);
  L_EXPRESSION2 VARCHAR2(32767) := WWV_FLOW.DO_SUBSTITUTIONS(p_validation_expression2);
  L_VALUE VARCHAR2(32767);
  l_boolean BOOLEAN;
  
  e_return_true EXCEPTION;
  e_return_false EXCEPTION;
  e_unknown_validation EXCEPTION;

  v_test_number NUMBER;
  v_test_date DATE;
  v_test_result VARCHAR2(3200);
BEGIN
  p_out_error_text := p_validation_error_text;
  L_VALUE := GET_VALUE(L_EXPRESSION1);
  
  CASE P_VALIDATION_TYPE
  WHEN 'EXISTS'                THEN IF selectCountFromQuery( L_EXPRESSION1 ) <= 0 THEN RAISE e_return_false; END IF;
  WHEN 'NOT_EXISTS'            THEN IF selectCountFromQuery( L_EXPRESSION1 ) >  0 THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_NOT_ZERO'         THEN IF NOT NVL(L_VALUE,'x') != '0'                THEN RAISE e_return_false; END IF;    
  WHEN 'ITEM_REQUIRED'         THEN    
       p_out_error_text := REPLACE(APEX_LANG.MESSAGE('APEX.PAGE_ITEM_IS_REQUIRED', L_EXPRESSION2), 'APEX.PAGE_ITEM_IS_REQUIRED', '#LABEL# must have some value.');
       IF L_VALUE IS NULL THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_NOT_NULL'         THEN IF L_VALUE IS NULL                            THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_NOT_NULL_OR_ZERO' THEN IF NVL(L_VALUE, '0') = '0'                    THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_ALPHANUMERIC'  THEN IF RTRIM(UPPER(L_VALUE), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_') IS NOT NULL THEN RAISE e_return_false; END IF;
  WHEN 'NATIVE_NUMBER_FIELD'   THEN
       p_out_error_text := REPLACE(APEX_LANG.MESSAGE('APEX.NUMBER_FIELD.VALUE_INVALID'), 'APEX.NUMBER_FIELD.VALUE_INVALID', '#LABEL# must be Numeric.');
       BEGIN
         v_test_number := TO_NUMBER( REPLACE(REPLACE(L_VALUE, ',', NULL), '.', NULL) );
       EXCEPTION
         WHEN OTHERS THEN RAISE e_return_false;
       END;
  WHEN 'ITEM_IS_NUMERIC'  THEN
       BEGIN
         v_test_number := TO_NUMBER( REPLACE(REPLACE(L_VALUE, ',', NULL), '.', NULL) );
       EXCEPTION
         WHEN OTHERS THEN RAISE e_return_false;
       END;
  WHEN 'ITEM_CONTAINS_NO_SPACES' THEN IF INSTR(L_VALUE,' ') > 0 THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IS_DATE' THEN
       IF L_VALUE IS NOT NULL THEN
         BEGIN
           v_test_date := TO_DATE(L_VALUE, NVL(getItemFormatMask(L_EXPRESSION1),'DD-MON-RR'));
         EXCEPTION
           WHEN others THEN RAISE e_return_false;
         END;
       END IF;
  WHEN 'NATIVE_DATE_PICKER' THEN
       p_out_error_text := REPLACE(APEX_LANG.MESSAGE('APEX.DATEPICKER_VALUE_INVALID', L_EXPRESSION2), 'APEX.DATEPICKER_VALUE_INVALID', '#LABEL# does not match format '||L_EXPRESSION2||'.');
       IF L_VALUE IS NOT NULL THEN
         BEGIN
           v_test_date := TO_DATE(L_VALUE, NVL(getItemFormatMask(L_EXPRESSION1),'DD-MON-RR'));
         EXCEPTION
           WHEN others THEN RAISE e_return_false;
         END;
       END IF;
  WHEN 'SQL_EXPRESION'    THEN IF selectCountFromQuery( 'select 1 from dual where '||perform_binds( L_EXPRESSION1, TRUE ) ) = 0 THEN RAISE e_return_false; END IF;
  WHEN 'SQL_EXPRESSION'   THEN IF selectCountFromQuery( 'select 1 from dual where '||perform_binds( L_EXPRESSION1, TRUE ) ) = 0 THEN RAISE e_return_false; END IF;
  WHEN 'PLSQL_EXPRESION'  THEN IF NOT get_plsql_expression_result( L_EXPRESSION1 ) THEN RAISE e_return_false; END IF;
  WHEN 'PLSQL_EXPRESSION' THEN IF NOT get_plsql_expression_result( L_EXPRESSION1 ) THEN RAISE e_return_false; END IF;
  WHEN 'PLSQL_ERROR'      THEN  RAISE e_return_false;
  WHEN 'FUNC_BODY_RETURNING_ERR_TEXT' THEN
       v_test_result := APEX_PLUGIN_UTIL.GET_PLSQL_FUNCTION_RESULT( L_EXPRESSION1 );
       IF v_test_result IS NOT NULL THEN
         p_out_error_text := v_test_result; 
         RAISE e_return_false;
       END IF;
  WHEN 'FUNC_BODY_RETURNING_BOOLEAN'       THEN IF NOT get_func_boolean_result( L_EXPRESSION1 ) THEN RAISE e_return_false; END IF;
  WHEN 'REGULAR_EXPRESSION'                THEN IF NOT REGEXP_LIKE (l_value, l_expression2)     THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IN_VALIDATION_IN_STRING2'     THEN IF INSTR(L_EXPRESSION2, L_VALUE) = 0            THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IN_VALIDATION_NOT_IN_STRING2' THEN IF INSTR(L_EXPRESSION2, L_VALUE) > 0            THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IN_VALIDATION_EQ_STRING2'     THEN IF NOT (L_EXPRESSION2 = l_value)                THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IN_VALIDATION_NOT_EQ_STRING2' THEN IF      L_EXPRESSION2 = L_VALUE                 THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IN_VALIDATION_CONTAINS_ONLY_CHAR_IN_STRING2' THEN IF RTRIM(L_VALUE, l_expression2) IS NOT NULL THEN RAISE e_return_false; END IF;
  WHEN 'ITEM_IN_VALIDATION_CONTAINS_AT_LEAST_ONE_CHAR_IN_STRING2' THEN
       IF l_value IS NULL THEN
          RAISE e_return_false;
       END IF;
       l_boolean := FALSE;
       FOR i IN 1..LENGTH(l_value) LOOP
         IF INSTR(l_expression2, SUBSTR(l_value, i, 1)) > 0 THEN
           l_boolean := TRUE;
           exit;
         END IF;
       END LOOP;    
       IF NOT l_boolean THEN
         RAISE e_return_false;
       END IF;
  WHEN 'ITEM_IN_VALIDATION_CONTAINS_NO_CHAR_IN_STRING2' THEN 
       IF l_value IS NULL THEN
         RAISE e_return_false;
       END IF;
       l_boolean := TRUE;
       FOR i IN 1..LENGTH(l_value) LOOP
         IF INSTR(l_expression2, SUBSTR(l_value, i, 1)) > 0 THEN
           l_boolean := FALSE;
           exit;
         END IF;
       END LOOP;
       IF NOT l_boolean THEN
         RAISE e_return_false;
       END IF;
  WHEN 'EXPRESSION' THEN
        CASE l_expression2
          WHEN 'PLSQL' THEN IF NOT get_plsql_expression_result( L_EXPRESSION1 ) THEN RAISE e_return_false; END IF;
          WHEN 'SQL'   THEN IF selectCountFromQuery( 'select 1 from dual where '||perform_binds( L_EXPRESSION1, TRUE ) ) = 0 THEN RAISE e_return_false; END IF;
          ELSE RAISE e_unknown_validation;
        END CASE;
  ELSE RAISE e_unknown_validation;
  END CASE;

  RAISE e_return_true;

EXCEPTION
  WHEN e_return_true THEN
    p_out_status := 1;
    p_out_msg := 'Validation "'|| P_VALIDATION_name ||'" ['|| P_VALIDATION_TYPE ||'] passed';
  WHEN e_return_false THEN
    p_out_status := 0;
    p_out_msg := 'Validation "'|| P_VALIDATION_name ||'" ['|| P_VALIDATION_TYPE ||'] failed';
  WHEN e_unknown_validation THEN
    p_out_status := 2;
    p_out_msg := 'Validation type ['|| P_VALIDATION_TYPE || '] not supported. Sorry';
    APEX_DEBUG.ERROR ( 'Validation type [%s] ("%s", "%s") not supported.', P_VALIDATION_TYPE, l_expression1, l_expression2);  
  WHEN OTHERS THEN
    p_out_status := 2;
    p_out_error_text := 'Error occured while performing validation: '||REPLACE(REPLACE(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, CHR(13), ' '), CHR(10), ' ');
    p_out_msg := 'Error occured while performing validation: '||REPLACE(REPLACE(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, CHR(13), ' '), CHR(10), ' ');
END VALIDATE;

PROCEDURE collectLogs(
  pi_names IN t_tab,
  pi_codes IN t_tab,
  pi_statuses IN t_tab,
  pi_msges IN t_tab,
  pi_conditions IN t_tab,
  po_exception OUT VARCHAR2,
  po_json OUT VARCHAR2
) IS
  v_logs_json VARCHAR2(32000);
BEGIN
  IF pi_names.count > 0 THEN
    FOR i IN pi_names.first..pi_names.last LOOP
      v_logs_json := v_logs_json||'{
        "validationCode": "'||pi_codes(i)||'",
        "validationName": "'||pi_names(i)||'",
        "validationMsg": "'||pi_msges(i)||'",
        "validationCondition": "'||pi_conditions(i)||'",
        "passed": '||pi_statuses(i)||'
      }';
    
      IF i <> pi_names.last THEN
        v_logs_json := v_logs_json||',';
      END IF;
    END LOOP;

  END IF;

  po_json := v_logs_json;

EXCEPTION
  WHEN OTHERS THEN
    po_exception := SQLERRM;
END collectLogs;

FUNCTION getFieldsToReValidate(
  p_item_name IN VARCHAR2
) RETURN VARCHAR2 IS
  v_items_to_validate VARCHAR2(3200);
BEGIN

  SELECT
    LISTAGG('#'||ASSOCIATED_ITEM, ',') WITHIN GROUP( ORDER BY 1 )
  INTO 
    v_items_to_validate
  FROM (
    SELECT 
      DISTINCT
        ASSOCIATED_ITEM
    FROM
      APEX_APPLICATION_PAGE_VAL aapv
    WHERE 
      application_id = :APP_ID
      AND page_id = :APP_PAGE_ID
      AND INSTR(':'||CONDITION_EXPRESSION1||CONDITION_EXPRESSION2||VALIDATION_EXPRESSION1||VALIDATION_EXPRESSION2||':', p_item_name) > 0
      AND ASSOCIATED_ITEM <> p_item_name  
  );

  RETURN v_items_to_validate;

END getFieldsToReValidate;

FUNCTION validation_result( 
  p_time IN NUMBER,
  p_item IN VARCHAR2,
  p_passed IN VARCHAR2,
  p_message IN VARCHAR2,
  p_revalidate IN VARCHAR2,
  p_logs IN VARCHAR2
) RETURN VARCHAR2 IS
  v_time_mask VARCHAR2(24) := '999G999G999G999G990D0000';
BEGIN
  RETURN '
    {
      "validation_result": {
        "time": {
          "ms": "'     ||REPLACE(TO_CHAR(p_time, v_time_mask),' ', '')||'",
          "seconds": "'||REPLACE(TO_CHAR((p_time/1000), v_time_mask),' ', '')||'",
          "minutes": "'||REPLACE(TO_CHAR((p_time/1000/60), v_time_mask),' ', '')||'"
        },
        "item": "'||p_item||'",
        "passed": '||p_passed||',
        "message": "'||p_message||'",
        "revalidate": "'||p_revalidate||'",
        "logs": ['||p_logs||']
      }
    }
  ';
END;

FUNCTION ajax_validation (
  p_dynamic_action IN apex_plugin.t_dynamic_action,
  p_plugin         IN apex_plugin.t_plugin 
) RETURN apex_plugin.t_dynamic_action_ajax_result
IS
  v_val_names t_tab;
  v_val_statuses t_tab;
  v_val_codes t_tab;
  v_val_msges t_tab;
  v_val_conditions t_tab;
  v_revalidate NUMBER := NVL(p_dynamic_action.attribute_08, 0);
  v_reval_fields VARCHAR2(4000);
  v_result apex_plugin.t_dynamic_action_ajax_result;
  v_item_id VARCHAR2(100) := apex_application.g_x01;
  v_validation_msg VARCHAR2(32000);

  v_cond_out_msg VARCHAR2(3200);
  v_cond_out_run NUMBER;

  v_val_out_msg VARCHAR2(3200);
  v_val_out_status NUMBER;

  v_label VARCHAR2(100);
  
  v_logs_json VARCHAR2(32000);

  v_log_exception VARCHAR2(32000);
  v_val_count NUMBER :=0;
  e_validation_not_found EXCEPTION;

  v_val_error_text VARCHAR2(32000);

  v_time_start NUMBER;
  v_time_end NUMBER;
  v_time_diff NUMBER;
BEGIN
  v_time_start := current_time_ms();

  IF v_revalidate = 1 THEN
    v_reval_fields := getFieldsToReValidate(v_item_id);
  END IF;

  BEGIN
     SELECT LABEL
       INTO v_label
       FROM APEX_APPLICATION_PAGE_ITEMS
      WHERE application_id = :APP_ID
        AND page_id = :APP_PAGE_ID
        AND ITEM_NAME = v_item_id
     ;
  EXCEPTION
    WHEN no_data_found THEN
      v_label := 'label not found';
    WHEN OTHERS THEN
      v_label := SQLERRM;
  END;

  FOR validation_row IN (
     SELECT *
       FROM ( SELECT VALIDATION_TYPE_CODE
                   , VALIDATION_name
                   , validation_expression1
                   , validation_expression2
                   , VALIDATION_FAILURE_TEXT
                   , VALIDATION_SEQUENCE
                   , CONDITION_TYPE
                   , CONDITION_TYPE_CODE
                   , CONDITION_EXPRESSION1
                   , CONDITION_EXPRESSION2
                FROM APEX_APPLICATION_PAGE_VAL aapv
               WHERE aapv.application_id  = :APP_ID
                 AND aapv.page_id         = :APP_PAGE_ID
                 AND aapv.ASSOCIATED_ITEM = v_item_id
                 AND (aapv.CONDITION_TYPE_CODE <> 'NEVER' OR aapv.CONDITION_TYPE_CODE IS NULL)
              UNION ALL
              SELECT DISPLAY_AS_CODE                                 VALIDATION_TYPE_CODE
                   , DISPLAY_AS_CODE                                 VALIDATION_name
                   , v_item_id                                       validation_expression1
                   , FORMAT_MASK                                     validation_expression2
                   , 'item built-in validation ' || DISPLAY_AS_CODE  VALIDATION_FAILURE_TEXT
                   , 999999999999                                    VALIDATION_SEQUENCE
                   , NULL                                            CONDITION_TYPE
                   , NULL                                            CONDITION_TYPE_CODE
                   , NULL                                            CONDITION_EXPRESSION1
                   , NULL                                            CONDITION_EXPRESSION2
                FROM APEX_APPLICATION_PAGE_ITEMS
               WHERE application_id = :APP_ID
                 AND page_id = :APP_PAGE_ID
                 AND ITEM_NAME = v_item_id
                 AND DISPLAY_AS_CODE IN ('NATIVE_NUMBER_FIELD', 'NATIVE_DATE_PICKER')
              UNION ALL
              SELECT 'ITEM_REQUIRED'
                   , 'ITEM_REQUIRED'
                   , v_item_id
                   , FORMAT_MASK
                   , 'item built-in validation: ITEM_REQUIRED'
                   , 999999999999
                   , NULL
                   , NULL
                   , NULL
                   , NULL
                FROM APEX_APPLICATION_PAGE_ITEMS
               WHERE application_id = :APP_ID
                 AND page_id        = :APP_PAGE_ID
                 AND ITEM_NAME      = v_item_id
                 AND UPPER ( IS_REQUIRED ) = 'YES'
            )
      ORDER BY VALIDATION_SEQUENCE ASC) 
  LOOP
    v_val_count := v_val_count +1;
    
    isRunConditionMatched(
      p_condition_type_code => validation_row.condition_type_code,
      p_condition_expression1 => validation_row.condition_expression1,
      p_condition_expression2 => validation_row.condition_expression2,
      p_out_run => v_cond_out_run,
      p_out_msg => v_cond_out_msg
    );

    IF v_cond_out_run IN (0,2) THEN
      v_val_names( v_val_names.count+1 ) := htf.escape_sc( validation_row.VALIDATION_name );
      v_val_codes( v_val_codes.count+1 ) := htf.escape_sc( validation_row.VALIDATION_TYPE_CODE );
      v_val_conditions( v_val_conditions.count+1 ) := htf.escape_sc( v_cond_out_msg );

      IF v_cond_out_run = 2 THEN
        v_val_msges( v_val_msges.count+1) := htf.escape_sc( 'Error occured in validation.');
        v_val_statuses( v_val_statuses.count+1 ) := '"error"';    
        v_validation_msg := 'Error occured in validation.';
        RAISE e_validation_failed; 
      ELSE
        v_val_statuses( v_val_statuses.count+1 ) := 'null';
        v_val_msges( v_val_msges.count+1) := htf.escape_sc( 'null' );
        CONTINUE;
      END IF;

    END IF;

    VALIDATE(
      P_VALIDATION_TYPE => validation_row.VALIDATION_TYPE_CODE,
      p_validation_name => validation_row.VALIDATION_name,
      p_validation_expression1 => validation_row.validation_expression1,
      p_validation_expression2 => validation_row.validation_expression2,
      p_validation_error_text => validation_row.VALIDATION_FAILURE_TEXT,
      p_out_error_text => v_val_error_text,
      p_out_status => v_val_out_status,
      p_out_msg => v_val_out_msg
    );
    
    v_val_msges( v_val_msges.count+1)  := htf.escape_sc(v_val_out_msg);
    v_val_names( v_val_names.count+1 ) := htf.escape_sc(validation_row.VALIDATION_name);
    v_val_codes( v_val_codes.count+1 ) := htf.escape_sc(validation_row.VALIDATION_TYPE_CODE);
    v_val_conditions( v_val_conditions.count+1 ) := htf.escape_sc( v_cond_out_msg );

    IF v_val_out_status = 1 THEN
      v_val_statuses( v_val_statuses.count+1 ) := 'true';
      CONTINUE;
    ELSIF v_val_out_status = 0 THEN
      v_val_statuses( v_val_statuses.count+1 ) := 'false';
      v_validation_msg := v_val_error_text;
      RAISE e_validation_failed; 
    ELSE --2
      v_val_statuses( v_val_statuses.count+1 ) := '"error"';
      v_validation_msg := 'Error occured while performing validation: ' || v_val_out_msg;
      RAISE e_validation_failed; 
    END IF;

  END LOOP;

  IF v_val_count = 0 THEN
    RAISE e_validation_not_found;
  END IF;

  collectLogs(v_val_names, v_val_codes, v_val_statuses, v_val_msges, v_val_conditions, v_log_exception, v_logs_json);

  IF v_log_exception IS NOT NULL THEN
    htp.p( v_log_exception );
  END IF;

  v_time_end := current_time_ms();
  v_time_diff := v_time_end - v_time_start;

  htp.p( validation_result(v_time_diff, v_item_id, 'true', 'Field is valid.', v_reval_fields, v_logs_json ) );

  RETURN v_result;
EXCEPTION
  WHEN e_validation_not_found THEN
    v_time_end := current_time_ms();
    v_time_diff := v_time_end - v_time_start;

    htp.p( validation_result(v_time_diff, v_item_id, CHR(34)||'not_found'||CHR(34), 'Validation for this field not found', v_reval_fields, '[]' ) );

    RETURN v_result;
  WHEN e_validation_failed THEN
    v_validation_msg := htf.escape_sc(v_validation_msg);
    v_validation_msg := REPLACE(v_validation_msg, CHR(35)||'LABEL'||CHR(35), v_label);
    v_validation_msg := REPLACE(v_validation_msg, CHR(13)||CHR(10), '\n');

    collectLogs(v_val_names, v_val_codes, v_val_statuses, v_val_msges, v_val_conditions, v_log_exception, v_logs_json);
    
    v_time_end := current_time_ms();
    v_time_diff := v_time_end - v_time_start;

    htp.p( validation_result(v_time_diff, v_item_id, 'false', v_validation_msg, v_reval_fields, v_logs_json ) );

    RETURN v_result;
  WHEN OTHERS THEN
    htp.p('ajax_validation: '||SQLERRM );
    RETURN v_result;
END ajax_validation;
