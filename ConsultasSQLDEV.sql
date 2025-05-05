--Consulta TotalClientes_Saldos (100% Clientes con Saldo)
SELECT C.*,S.* from(select
a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT "RFC"
from GA_ABOCEL a
left join ge_clientes b on a.COD_CLIENTE = b.COD_CLIENTE
left join ge_categorias c on b.COD_CATEGORIA = c.COD_CATEGORIA and a.COD_CUENTA = b.COD_CUENTA
left join ga_cuentas d on a.COD_CUENTA = d.COD_CUENTA
group by a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT)C
inner join (
SELECT S.* from(SELECT d.cod_cliente, SUM(CASE WHEN(d.importe_debe - d.importe_haber) <> 0 THEN d.importe_debe - d.importe_haber END) saldo,
SUM(CASE WHEN d.cod_tipdocum = 39 Then d.importe_debe - d.importe_haber END) castigo
FROM co_cartera d
GROUP BY d.cod_cliente)S )S on C.COD_CLIENTE = S.COD_CLIENTE	
---- Consulta 2
SELECT C.*,S.* from(select
a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT "RFC"
from GA_ABOCEL a
left join ge_clientes b on a.COD_CLIENTE = b.COD_CLIENTE
left join ge_categorias c on b.COD_CATEGORIA = c.COD_CATEGORIA and a.COD_CUENTA = b.COD_CUENTA
left join ga_cuentas d on a.COD_CUENTA = d.COD_CUENTA
group by a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT)C
inner join (
SELECT S.* from(SELECT d.cod_cliente, SUM(CASE WHEN(d.importe_debe - d.importe_haber) != ALL (0) THEN d.importe_debe - d.importe_haber END) saldo,
SUM(CASE WHEN d.cod_tipdocum = 39 Then d.importe_debe - d.importe_haber END) castigo
FROM co_cartera d
GROUP BY d.cod_cliente)S )S on C.COD_CLIENTE = S.COD_CLIENTE

----Consulta CoPagos_CONC para ejecutivos
SELECT
    a.cod_cliente, b.*
FROM
    co_pagos       a
    INNER JOIN co_pagosconc   b ON a.num_secuenci = b.num_secuenci
                                 AND a.cod_tipdocum = b.cod_tipdocum
WHERE
    a.cod_cliente IN (
        12165837,77805792)
------


---OK-- Consulta para obtener primero la cartera con saldos
SELECT S.*,C.* from(
SELECT s.* FROM (SELECT d.cod_cliente, SUM(DISTINCT d.importe_debe - d.importe_haber) saldo,
            SUM(DISTINCT CASE WHEN d.cod_tipdocum = 39 THEN d.importe_debe - d.importe_haber END) castigo
        FROM co_cartera d
        GROUP BY d.cod_cliente
        HAVING Sum(DISTINCT(d.IMPORTE_DEBE - d.importe_haber)) <> 0
        ORDER BY d.cod_cliente) s )S
left join (
select
a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT "RFC"
from GA_ABOCEL a
left join ge_clientes b on a.COD_CLIENTE = b.COD_CLIENTE
left join ge_categorias c on b.COD_CATEGORIA = c.COD_CATEGORIA and a.COD_CUENTA = b.COD_CUENTA
left join ga_cuentas d on a.COD_CUENTA = d.COD_CUENTA
group by a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT)C
on S.COD_CLIENTE = C.COD_CLIENTE

---OK- Consulta con saldo 0 y castigos
SELECT S.*,C.* from(
SELECT s.* FROM (SELECT d.cod_cliente, SUM(DISTINCT d.importe_debe - d.importe_haber) saldo,
            SUM(DISTINCT CASE WHEN d.cod_tipdocum = 39 THEN d.importe_debe - d.importe_haber END) castigo
        FROM co_cartera d
        GROUP BY d.cod_cliente
        HAVING Sum(DISTINCT(d.IMPORTE_DEBE - d.importe_haber)) = 0
        ORDER BY d.cod_cliente) s )S
left join (
select
a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT "RFC"
from GA_ABOCEL a
left join ge_clientes b on a.COD_CLIENTE = b.COD_CLIENTE
left join ge_categorias c on b.COD_CATEGORIA = c.COD_CATEGORIA and a.COD_CUENTA = b.COD_CUENTA
left join ga_cuentas d on a.COD_CUENTA = d.COD_CUENTA
group by a.COD_CLIENTE,b.NOM_CLIENTE,a.COD_CUENTA,d.DES_CUENTA,a.COD_CICLO,b.COD_CATEGORIA,c.DES_CATEGORIA,
a.COD_SITUACION, a.COD_ESTADO,b.NUM_IDENT)C
on S.COD_CLIENTE = C.COD_CLIENTE


--Consulta TotalClientes_Saldos por Cajon(100% Clientes con Saldo)

SELECT
    a.cod_cliente,
    a.importe_debe,
    a.importe_haber,
    ( a.importe_debe - a.importe_haber ) importe,
    a.cod_tipdocum,
    round(SYSDATE - a.fec_vencimie, 0) dias,
    CASE
        WHEN round(SYSDATE - a.fec_vencimie, 0) <= 0 THEN
            'CORRIENTE'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1
             AND round(SYSDATE - a.fec_vencimie, 0) <= 30 THEN
            'D_1_A_30'
        WHEN round(SYSDATE - a.FEC_VENCIMIE, 0) >= 31
             AND round(SYSDATE - a.fec_vencimie, 0) <= 60 THEN
            'D_31_A_60'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
             AND round(SYSDATE - a.fec_vencimie, 0) <= 90 Then
            'D_61_A_90'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91 And round(SYSDATE - a.fec_vencimie, 0) <= 120 THEN
            'D_91_A_120'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
             AND round(SYSDATE - a.fec_vencimie, 0) <= 150 THEN
            'D_121_A_150'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151 And round(SYSDATE - a.fec_vencimie, 0) <= 180 THEN
            'D_151_A_180'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
             AND round(SYSDATE - a.fec_vencimie, 0) <= 210 THEN
            'D_181_A_210'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
             AND round(SYSDATE - a.fec_vencimie, 0) <= 240 THEN
            'D_211_A_240'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
             AND round(SYSDATE - a.fec_vencimie, 0) <= 360 THEN
            'D_241_A_360'
        WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
            'MAS_360'
    END cajon
FROM
    co_cartera a
WHERE
    a.cod_tipdocum != ALL (
        32
    )
GROUP BY
    a.cod_cliente,
    a.importe_debe,
    a.importe_haber,
    ( a.importe_debe - a.importe_haber ), a.cod_tipdocum, round(SYSDATE - a.fec_vencimie, 0),
    CASE
            WHEN round(SysDate - a.FEC_VENCIMIE, 0) <= 0 THEN
                'CORRIENTE'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 30 THEN
                'D_1_A_30'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 31
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 60 THEN
                'D_31_A_60'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 90 THEN
                'D_61_A_90'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 120 THEN
                'D_91_A_120'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 150 THEN
                'D_121_A_150'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 180 THEN
                'D_151_A_180'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 210 THEN
                'D_181_A_210'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
                 AND round(SysDate - a.FEC_VENCIMIE, 0) <= 240 THEN
                'D_211_A_240'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
                 AND round(SysDate - a.fec_vencimie, 0) <= 360 THEN
                'D_241_A_360'
            WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
                'MAS_360'
        END
ORDER BY
    a.cod_cliente


-----Modelo Predictivo
Medir: Comparar una cantidad con su respectiva unidad, con el fin de averiguar cuántas veces la segunda esta contenida
en la primera.
Patrón(Unidad): Cantidad que se toma por medida o término de comparación de las demás de su especie.
Error: la diferencia entre el valor medido y el valor calculado.
Errores: Sistemáticos (siempre afectan de la misma forma al resultado de la medida,a veces se puede corregir, y siempre se pueden acotar)
Accidentales (Afectan de forma aleatoria, No se pueden corregir, se estudian y acotan estadísticamente)

---100% de la facturacion
SELECT A.COD_CLIENTE, A.COD_TIPDOCUM, A.TOT_FACTURA, A.TOT_PAGAR, A.FEC_EMISION, A.FEC_VENCIMIE,
C.FEC_VALOR, C.FEC_VALOR, C.IMP_PAGO, C.DES_PAGO
FROM FA_HISTDOCU A, CO_PAGOS C 
WHERE A.COD_CLIENTE = C.COD_CLIENTE
AND A.COD_TIPDOCUM = C.COD_TIPDOCUM



select  cod_cliente, cod_tipdocum,
tot_factura, tot_pagar, fec_emision, fec_vencimie
from FA_HISTDOCU,
where cod_cliente = 66274834

SELECT COD_CLIENTE, COD_TIPDOCUM, FEC_VALOR, FEC_VALOR, IMP_PAGO, DES_PAGO
FROM CO_PAGOS


SELECT COD_CLIENTE, COD_TIPDOCUM, fec_efectividad,
fec_vencimie, fec_caducida, fec_antiguedad, 
importe_debe, importe_haber


FROM CO_CARTERA a, CO_PAGOS

select COD_CLIENTE, COD_TIPDOCUM, FEC_EFECTIVIDAD,
FEC_VALOR, DES_PAGO, IMP_PAGO
from co_pagos


co_pagos co_cancelados co_cartera co_canceladoconc	


-----Fin Modelo Predictivo



--tablas
FROM CO_CARTERA --cobranza--
from ga_abocel /*tabla para checar estatus*/
------Productivo Checar
select * 
from all_tables@scl_prod.tauro
where lower(table_name) like '%prueba24%' 
------

---fin tablas

select * 
from all_all_tables
WHERE owner= 'SISCEL'


---Fechas en SCL---
SELECT COD_CLIENTE, COD_TIPDOCUM, fec_efectividad,
fec_vencimie, fec_caducida, fec_antiguedad, 
importe_debe, importe_haber
FROM CO_CARTERA
where cod_cliente= 80396602

select cod_cliente, count(distinct(cod_situacion))
from ga_abocel
where cod_cliente= 67432869
group by cod_cliente

select cod_situacion
from ga_abocel /*tabla para checar estatus*/
where cod_cliente= 90386702


select cod_cliente, cod_situacion
from ga_abocel
where cod_cliente= 67432869
group by cod_cliente, cod_situacion

--

-----CONSULTA Total Cliente con saldo para identificar cuandos codigos tienen las cuentas 

SELECT S.*,C.* from(
SELECT s.* FROM (SELECT d.cod_cliente, SUM(DISTINCT d.importe_debe - d.importe_haber) saldo,
            SUM(DISTINCT CASE WHEN d.cod_tipdocum = 39 THEN d.importe_debe - d.importe_haber END) castigo
        FROM co_cartera d
        GROUP BY d.cod_cliente
        HAVING Sum(DISTINCT(d.IMPORTE_DEBE - d.importe_haber)) > 0
        ORDER BY d.cod_cliente) s )S
left join (
SELECT
    c.cod_cuenta    cuenta,
    d.des_cuenta,
    a.cod_cliente,
    c.nom_cliente   nombre,
    a.num_cuotas
FROM
    fa_histdocu   a
    LEFT JOIN ge_clientes   c ON a.cod_cliente = c.cod_cliente
    LEFT JOIN ga_cuentas    d ON c.cod_cuenta = d.cod_cuenta
GROUP BY
    c.cod_cuenta,
    d.des_cuenta,
    a.cod_cliente,
    c.nom_cliente,
    a.NUM_CUOTAS
    Order By cuenta )c
    on S.COD_CLIENTE = C.COD_CLIENTE




----Consulta SIMIL FONSECA PARA CARTERA CONSOLIDADA
SELECT CAR.* from (
SELECT
    a.cod_cliente Cliente,
    b.nom_cliente Nombre,
    b.cod_cuenta Cuenta,
    d.des_cuenta,
    a.cod_tipdocum,
    a.pref_plaza,
    a.num_folio,
    SUM(a.importe_debe) AS Debe,
    SUM(a.importe_haber) AS Haber,
    SUM((a.importe_debe - a.importe_haber)) Saldo,
    a.fec_efectividad,
    a.fec_vencimie,
    c.des_categoria,
    round(SYSDATE - a.fec_vencimie, 0) dias,
    CASE
        WHEN round(SYSDATE - a.fec_vencimie, 0) <= 0 THEN
            'CORRIENTE'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1
             AND round(SYSDATE - a.fec_vencimie, 0) <= 30 THEN
            'D_01_30'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 31
             AND round(SYSDATE - a.fec_vencimie, 0) <= 60 THEN
            'D_31_60'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
             AND round(SYSDATE - a.fec_vencimie, 0) <= 90 THEN
            'D_61_90'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91
             AND round(SYSDATE - a.fec_vencimie, 0) <= 120 THEN
            'D_91_120'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
             AND round(SYSDATE - a.fec_vencimie, 0) <= 150 THEN
            'D_121_150'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151
             AND round(SYSDATE - a.fec_vencimie, 0) <= 180 THEN
            'D_151_180'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
             AND round(SYSDATE - a.fec_vencimie, 0) <= 210 THEN
            'D_181_210'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
             AND round(SYSDATE - a.fec_vencimie, 0) <= 240 THEN
            'D_211_240'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
             AND round(SYSDATE - a.fec_vencimie, 0) <= 360 THEN
            'D_241_360'
        WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
            'MAS_360'
    END cajon,
    CASE
        WHEN round((SYSDATE - a.fec_vencimie), 0) <= '0'
             AND round((SYSDATE - a.fec_vencimie), 0) < '1' THEN
            'CORRIENTE'
        ELSE
            'VENCIDO'
    END AS cajonm,
    CASE
        WHEN a.cod_tipdocum = '1' THEN
            'EQUIPO'
        ELSE
            'SERVICIO'
    END AS eq_serv,
    CASE
        WHEN a.cod_tipdocum = 39 THEN
            a.importe_debe - a.importe_haber
    END castigo
FROM
    co_cartera      a
    LEFT JOIN ge_clientes     b ON a.cod_cliente = b.cod_cliente
    LEFT JOIN ge_categorias   c ON b.cod_categoria = c.cod_categoria
    LEFT JOIN ga_cuentas      d ON b.cod_cuenta = d.cod_cuenta
/*WHERE
    a.cod_cliente in( 20990330, 20977027,20977368,20980283,20981963,66875280)*/

GROUP BY
    a.cod_cliente,
    b.nom_cliente,
    b.cod_cuenta,
    d.des_cuenta,
    a.cod_tipdocum,
    a.pref_plaza,
    a.num_folio,
    a.fec_efectividad,
    a.fec_vencimie,
    c.des_categoria,
    round(SYSDATE - a.fec_vencimie, 0),
    CASE
            WHEN round(SYSDATE - a.fec_vencimie, 0) <= 0 Then
                'CORRIENTE'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1 And round(SysDate - a.fec_vencimie, 0) <= 30 THEN
                'D_1_A_30'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 31
                 AND round(SysDate - a.fec_vencimie, 0) <= 60 THEN
                'D_31_A_60'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
                 AND round(SysDate - a.fec_vencimie, 0) <= 90 THEN
                'D_61_A_90'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91
                 AND round(SysDate - a.fec_vencimie, 0) <= 120 THEN
                'D_91_A_120'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
                 AND round(SysDate - a.fec_vencimie, 0) <= 150 THEN
                'D_121_A_150'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151
                 AND round(SysDate - a.fec_vencimie, 0) <= 180 THEN
                'D_151_A_180'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
                 AND round(SysDate - a.fec_vencimie, 0) <= 210 THEN
                'D_181_A_210'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
                 AND round(SysDate - a.fec_vencimie, 0) <= 240 Then
                'D_211_A_240'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
                 AND round(SysDate - a.fec_vencimie, 0) <= 360 THEN
                'D_241_A_360'
            WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
                'MAS_360'
        END,
    CASE
            WHEN round((SYSDATE - a.fec_vencimie), 0) <= '0' And round((SysDate - a.fec_vencimie), 0) < '1' THEN
                'CORRIENTE' Else
                'VENCIDO'
        END,
    CASE
            WHEN a.cod_tipdocum = '1' THEN
                'EQUIPO'
            ELSE
                'SERVICIO'
        END,
    CASE
            WHEN a.cod_tipdocum = 39 THEN
                a.importe_debe - a.importe_haber
        END,
    CASE
            WHEN round(SYSDATE - a.fec_vencimie, 0) <= 0 THEN
                'CORRIENTE'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 30 THEN
                'D_1_A_30'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 31
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 60 THEN
                'D_31_A_60'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 90 THEN
                'D_61_A_90'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 120 THEN
                'D_91_A_120'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 150 THEN
                'D_121_A_150'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 180 THEN
                'D_151_A_180'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 210 THEN
                'D_181_A_210'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 240 THEN
                'D_211_A_240'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 360 Then
                'D_241_A_360'
            WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
                'MAS_360'
            WHEN a.cod_tipdocum = 39                   THEN
                'CASTIGO'
        END) CAR
       



    SELECT
    pena.*
FROM
    (
        SELECT
            b.cod_cliente,
            a.des_concepto PENA
            
        FROM
            fa_histconc_19010102   a
            LEFT JOIN fa_histdocu            b ON a.ind_ordentotal = b.ind_ordentotal
            LEFT JOIN ge_clientes            c ON b.cod_cliente = c.cod_cliente
        WHERE
            a.cod_concepto IN (
                25298, 25295,
                188,
                26086,
                26088
            )
            AND b.cod_tipdocum IN (
                18
            )
    ) pena
	
--CONSULTA para	el analisis por factura de Sergio

--Facturas y Notas de Credito
SELECT
    fa_histdocu.cod_cliente,
    fa_histdocu.cod_tipdocum,
    fa_histdocu.pref_plaza,
    fa_histdocu.num_folio,
    fa_histdocu.tot_pagar,
    fa_histdocu.tot_factura,
    fa_histdocu.fec_emision,
    fa_histdocu.fec_vencimie,
    fa_histdocu.fec_caducida
FROM
    fa_histdocu
WHERE
    /*fa_histdocu.cod_cliente IN (
        10880000
    )
    AND */ fa_histdocu.fec_emision BETWEEN '01-05-2019' AND '28-08-2019'
ORDER BY
    fa_histdocu.fec_emision

---
SELECT
    co_viewcartera.cod_cliente,
    co_viewcartera.num_folio,
    SUM(co_viewcartera.importe_debe) AS sum_importe_debe,
    SUM(co_viewcartera.importe_haber) AS sum_importe_haber,
    co_viewcartera.cod_tipdocum,
    co_viewcartera.num_secuenci,
    co_viewcartera.ind_facturado,
    co_viewcartera.fec_efectividad,
    co_viewcartera.fec_vencimie,
    co_viewcartera.pref_plaza,
    co_viewcartera.num_cuota,
    SUM(co_viewcartera.importe_debe - co_viewcartera.importe_haber) "Saldo",
    co_viewcartera.sec_cuota,
    MAX(DISTINCT co_viewcartera.fec_pago) AS max_fec_pago
FROM
    co_viewcartera
Where fec_efectividad between '01-07-2019' and '31-07-2019'
and cod_tipdocum in(1,2,18)/*se elimino ,89,9,10,25 para cuadrar con Fonseca(2 y 18) el 1 se deja porque interesa equipo*/
GROUP By
    co_viewcartera.cod_cliente,
    co_viewcartera.num_folio,
    co_viewcartera.COD_TIPDOCUM,
    co_viewcartera.num_secuenci,
    co_viewcartera.ind_facturado,
    co_viewcartera.fec_efectividad,
    co_viewcartera.fec_vencimie,
    co_viewcartera.pref_plaza,
    co_viewcartera.num_cuota,
    co_viewcartera.sec_cuota

--pagos
select COD_CLIENTE,COD_TIPDOCUM,IMP_PAGO,FEC_EFECTIVIDAD,FEC_VALOR,DES_PAGO
from co_pagos
where FEC_EFECTIVIDAD between '01-08-2019' and '31-08-2019'
	
---Consulta Distribuidores
select a.num_abonado, a.num_celular, a.cod_cliente,a.fec_alta,a.fec_baja,a.cod_situacion,a.COD_CAUSABAJA,a.COD_MODVENTA,
a.COD_VENDEDOR,a.COD_VENDEDOR_AGENTE,a.IND_PORTADO,a.FEC_FINCONTRA,a.COD_PLANTARIF,a.IND_PROCEQUI,
b.NUM_IDENT,b.NOM_CLIENTE,b.NOM_APECLIEN1,b.NOM_APECLIEN2,b.COD_CATEGORIA,c.DES_CATEGORIA,a.COD_CICLO,d.DES_CICLO
from ga_abocel a
left join ge_clientes b on a.COD_CLIENTE = b.COD_CLIENTE
left join ge_categorias c on b.COD_CATEGORIA = c.COD_CATEGORIA
left join FA_CICLOS d on  a.COD_CICLO = d.COD_CICLO
where cod_vendedor_agente IN (201372,157845) 
GROUP BY
    a.num_abonado,a.num_celular,a.cod_cliente,a.fec_alta,a.fec_baja,a.cod_situacion,a.cod_causabaja,
    a.cod_modventa,a.cod_vendedor,a.COD_VENDEDOR_AGENTE, a.IND_PORTADO,a.fec_fincontra,
    a.COD_PLANTARIF,a.ind_procequi,b.num_ident,b.nom_cliente,b.nom_apeclien1,
    b.nom_apeclien2,b.cod_categoria,c.des_categoria,a.cod_ciclo,d.des_ciclo
        
		
----SALDO Distribuidores
		
	SELECT
    a.num_abonado,
    a.cod_cliente,
    SUM(a.importe_debe) AS debe,
    SUM(a.importe_haber) AS haber,
    SUM((a.importe_debe - a.importe_haber)) saldo,
    CASE
        WHEN round(SYSDATE - a.fec_vencimie, 0) <= 0 THEN
            'CORRIENTE'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1
             AND round(SYSDATE - a.fec_vencimie, 0) <= 30 THEN
            'D_01_30'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 31
             AND round(SysDate - a.fec_vencimie, 0) <= 60 THEN
            'D_31_60'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
             AND round(SYSDATE - a.fec_vencimie, 0) <= 90 THEN
            'D_61_90'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91
             AND round(SYSDATE - a.fec_vencimie, 0) <= 120 THEN
            'D_91_120'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
             AND round(SYSDATE - a.fec_vencimie, 0) <= 150 THEN
            'D_121_150'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151
             AND round(SYSDATE - a.fec_vencimie, 0) <= 180 THEN
            'D_151_180'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
             AND round(SYSDATE - a.fec_vencimie, 0) <= 210 THEN
            'D_181_210'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
             AND round(SYSDATE - a.fec_vencimie, 0) <= 240 THEN
            'D_211_240'
        WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
             AND round(SYSDATE - a.fec_vencimie, 0) <= 360 THEN
            'D_241_360'
        WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
            'MAS_360'
    END cajon,
    CASE
        WHEN round((SYSDATE - a.fec_vencimie), 0) <= '0'
             AND round((SYSDATE - a.fec_vencimie), 0) < '1' THEN
            'CORRIENTE'
        ELSE
            'VENCIDO'
    END AS cajonm,
    CASE
        WHEN a.cod_tipdocum = '1' THEN
            'EQUIPO'
        ELSE
            'SERVICIO'
    END AS eq_serv,
    CASE
        WHEN a.cod_tipdocum = 39 THEN
            a.importe_debe - a.importe_haber
    END castigo
FROM
    co_cartera a
WHERE
    a.cod_vendedor_agente IN (
        201372,
        157845
    )
GROUP BY
    a.num_abonado,
    a.cod_cliente,
    CASE
            WHEN round(SYSDATE - a.fec_vencimie, 0) <= 0 THEN
                'CORRIENTE'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 1
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 30 THEN
                'D_01_30'
            WHEN round(SysDate - a.fec_vencimie, 0) >= 31
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 60 THEN
                'D_31_60'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 61
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 90 THEN
                'D_61_90'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 91
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 120 THEN
                'D_91_120'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 121
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 150 THEN
                'D_121_150'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 151
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 180 THEN
                'D_151_180'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 181
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 210 THEN
                'D_181_210'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 211
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 240 THEN
                'D_211_240'
            WHEN round(SYSDATE - a.fec_vencimie, 0) >= 241
                 AND round(SYSDATE - a.fec_vencimie, 0) <= 360 THEN
                'D_241_360'
            WHEN round(SYSDATE - a.fec_vencimie, 0) > 360 THEN
                'MAS_360'
        END,
    CASE
            WHEN round((SYSDATE - a.fec_vencimie), 0) <= '0'
                 AND round((SYSDATE - a.fec_vencimie), 0) < '1' THEN
                'CORRIENTE'
            ELSE
                'VENCIDO'
        END,
    CASE
            WHEN a.cod_tipdocum = '1' THEN
                'EQUIPO'
            ELSE
                'SERVICIO'
        END,
    CASE
            WHEN a.cod_tipdocum = 39 THEN
                a.importe_debe - a.importe_haber
        END
		
---Query 	
SELECT
    a.cod_cliente,
    a.num_ident rfc,
    b.cod_situacion
FROM
    ge_clientes   a
    INNER JOIN ga_abocel b ON a.cod_cliente = b.cod_cliente
GROUP BY
    a.cod_cliente,
    a.num_ident,
    b.cod_situacion
HAVING
    a.cod_cliente = 3684770
	
---Regiones
SELECT
    ga_abocel.cod_cliente,
    ga_abocel.cod_region,
    ge_regiones.des_region
FROM
    ga_abocel left
    JOIN ge_regiones ON ga_abocel.cod_region = ge_regiones.cod_region
GROUP BY
    ga_abocel.cod_cliente,
    ga_abocel.cod_region,
    ge_regiones.des_region
ORDER BY
    ga_abocel.cod_cliente