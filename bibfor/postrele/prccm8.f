      SUBROUTINE PRCCM8 ( NOMRES, NBFT, NBORDR, PARASG, COURBE, LINTI,
     +                    INTITU, SM, NCHEFF )
      IMPLICIT   NONE
      INTEGER             NBFT, NBORDR
      REAL*8              SM
      LOGICAL             LINTI
      CHARACTER*16        NCHEFF
      CHARACTER*(*)       NOMRES, PARASG, COURBE, INTITU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/05/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C     POST_RCCM: ON REMPLIT LA TABLE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IDSNO, IDSNE, IDSPO, IDSPE, IDNBC1, IDNBC2, IDNBC3,
     +              IDNBC4, IDTEM1, IDTEM2, IDOCC1, IDOCC2, IDKEO, 
     +              IDKEE, IDSATO, IDSATE, IDNADO, IDNADE, IDUSAO, I,
     +              IDUSAE, NBCYCO, NBCYCE, VALOI(2), VALEI(2), IOCC, J,
     +              IFR, IUNIFI, N1, NBCHEF, JNUME, NBLIGN, NPARA, 
     +              INDORI, INDEXT, INDIRO, INDIRE, IK, IK2
      REAL*8        ZERO, DIX, DOMMCO, DOMMCE, R8PREM ,
     +              VALOR(30), VALER(30)
      COMPLEX*16    C16B
      CHARACTER*8   K8B, RESMEC, TYPARA(30)
      CHARACTER*16  VALOK(5), VALEK(5), NOPARA(30), TYPTAB
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      ZERO =  0.0D0
      DIX  = 10.0D0
      TYPTAB = 'VALE_MAX'
C
      IK = 1
      NPARA  = 1
      NOPARA(NPARA) = PARASG
      VALOK(IK) = COURBE
      VALEK(IK) = COURBE
C
      IK = IK + 1
      NPARA  = NPARA + 1
      NOPARA(NPARA) = 'LIEU'
      VALOK(IK) = 'ORIG'
      VALEK(IK) = 'EXTR'
C
      IF ( LINTI ) THEN
         IK = IK + 1
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'INTITULE'
         VALOK(IK) = INTITU
         VALEK(IK) = INTITU
      ENDIF
C
      IK = IK + 1
      IK2 = IK + 1
C
      CALL PRCCM9 ( .FALSE., .FALSE., .FALSE., .TRUE., .FALSE.,
     +                       TYPTAB, NPARA, NOPARA, TYPARA )
C
      VALOR(1) = SM
      VALOR(2) = 3*SM
C
      VALER(1) = SM
      VALER(2) = 3*SM
C
      CALL JEVEUO ( '&&OP0165.SNO'     , 'L', IDSNO  )
      CALL JEVEUO ( '&&OP0165.SNE'     , 'L', IDSNE  )
      CALL JEVEUO ( '&&OP0165.SPO'     , 'L', IDSPO  )
      CALL JEVEUO ( '&&OP0165.SPE'     , 'L', IDSPE  )
      CALL JEVEUO ( '&&OP0165.NBCYCL1' , 'E', IDNBC1 )
      CALL JEVEUO ( '&&OP0165.NBCYCL2' , 'E', IDNBC2 )
      CALL JEVEUO ( '&&OP0165.NBCYCL3' , 'E', IDNBC3 )
      CALL JEVEUO ( '&&OP0165.NBCYCL4' , 'E', IDNBC4 )
      CALL JEVEUO ( '&&OP0165.NUMINST1', 'L', IDTEM1 )
      CALL JEVEUO ( '&&OP0165.NUMINST2', 'L', IDTEM2 )
      CALL JEVEUO ( '&&OP0165.NOMRESU1', 'L', IDOCC1 )
      CALL JEVEUO ( '&&OP0165.NOMRESU2', 'L', IDOCC2 )
      CALL JEVEUO ( '&&OP0165.KEO'     , 'L', IDKEO  )
      CALL JEVEUO ( '&&OP0165.KEE'     , 'L', IDKEE  )
      CALL JEVEUO ( '&&OP0165.SALTO'   , 'L', IDSATO )
      CALL JEVEUO ( '&&OP0165.SALTE'   , 'L', IDSATE )
      CALL JEVEUO ( '&&OP0165.NADMO'   , 'L', IDNADO )
      CALL JEVEUO ( '&&OP0165.NADME'   , 'L', IDNADE )
      CALL JEVEUO ( '&&OP0165.USAGEO'  , 'L', IDUSAO )
      CALL JEVEUO ( '&&OP0165.USAGEE'  , 'L', IDUSAE )
C
      CALL WKVECT('&&PRCCM8.INDIRO','V V I',NBORDR,INDIRO)
      CALL WKVECT('&&PRCCM8.INDIRE','V V I',NBORDR,INDIRE)
C
C --- DETERMINATION DU TABLEAU D'INDIRECTION DU TABLEAU DES
C --- FACTEURS D'USAGE A L'ORIGINE VERS CE TABLEAU CLASSE
C --  PAR ORDRE CROISSANT :
C     -------------------
      CALL ORDR8 ( ZR(IDSATO), NBORDR, ZI(INDIRO) )
C
C --- DETERMINATION DU TABLEAU D'INDIRECTION DU TABLEAU DES
C --- FACTEURS D'USAGE A L'EXTREMITE VERS CE TABLEAU CLASSE
C --- PAR ORDRE CROISSANT :
C     -------------------
      CALL ORDR8(ZR(IDSATE),NBORDR,ZI(INDIRE))
C
      DOMMCO = ZERO
      DOMMCE = ZERO
C
C --- CUMUL DES FACTEURS D'USAGE POUR LE CALCUL DE L'ENDOMMAGEMENT :
C     ------------------------------------------------------------
      DO 10 I = NBORDR, 1, -1
C
         INDORI = ZI(INDIRO+I-1)
         INDEXT = ZI(INDIRE+I-1)
C
         VALOK(IK) = ZK8(IDOCC1+INDORI-1)
         VALOR(3)  = ZR (IDTEM1+INDORI-1)
         VALEK(IK) = ZK8(IDOCC1+INDEXT-1)
         VALER(3)  = ZR (IDTEM1+INDEXT-1)
C
         NBCYCO = MAX( ZI(IDNBC1+INDORI-1) , ZI(IDNBC2+INDORI-1) )
         NBCYCE = MAX( ZI(IDNBC3+INDEXT-1) , ZI(IDNBC4+INDEXT-1) )
C
C ---    ON NE FAIT LE CUMUL QUE SI LES NOMBRES DE CYCLES 
C ---    ASSOCIES A LA COMBINAISON DES 2 CHARGEMENTS COURANTE
C ---    SONT NON-NULS.
C
C ---    TRAITEMENT DE L'ORIGINE DU CHEMIN :
C        ---------------------------------
         IF ( ZI(IDNBC1+INDORI-1).NE.0 .AND. 
     +                           ZI(IDNBC2+INDORI-1).NE.0 ) THEN
C
C ---       CUMUL :
C           -----
            DOMMCO = DOMMCO + ZR(IDUSAO+INDORI-1)
C 
C ---       AFFECTATION DU TABLEAU A ECRIRE DANS LA TABLE :
C           ---------------------------------------------
            VALOK(IK2) = ZK8(IDOCC2+INDORI-1)
            VALOR(4)  = ZR(IDTEM2+INDORI-1)
            VALOR(5)  = ZR(IDSNO+INDORI-1)
            VALOR(6)  = ZR(IDSPO+INDORI-1)
            VALOR(7)  = ZR(IDKEO+INDORI-1)
            VALOR(8)  = ZR(IDSATO+INDORI-1)
            VALOR(9)  = ZR(IDNADO+INDORI-1)
            VALOI(1)  = ZI(IDNBC1+INDORI-1)
            VALOI(2)  = ZI(IDNBC2+INDORI-1)
            VALOR(10) = ZR(IDUSAO+INDORI-1)
            IF (VALOR(10).LE.DIX*R8PREM())  VALOR(10) = ZERO
            VALOR(11) = DOMMCO
C
C ---       REACTUALISATION DU NOMBRE DE CYCLES SELON LE PRINCIPE :
C ---         SI NBCYCL1 < NBCYCL2  
C ---                NBCYCL2 = NBCYCL2 - NBCYCL1
C ---                NBCYCL1 = 0
C ---         SINON  
C ---                NBCYCL1 = NBCYCL1 - NBCYCL2
C ---                NBCYCL2 = 0 :
C           ----------------------
            IF ( ZI(IDNBC1+INDORI-1) .LT. ZI(IDNBC2+INDORI-1)) THEN
               ZI(IDNBC2+INDORI-1) = ZI(IDNBC2+INDORI-1) - 
     +                                             ZI(IDNBC1+INDORI-1)
               ZI(IDNBC1+INDORI-1) = 0
            ELSE
               ZI(IDNBC1+INDORI-1) = ZI(IDNBC1+INDORI-1) - 
     +                                             ZI(IDNBC2+INDORI-1)
               ZI(IDNBC2+INDORI-1) = 0
            ENDIF
         ENDIF
C
C
C ---   TRAITEMENT DE L'EXTREMITE DU CHEMIN :
C       -----------------------------------
        IF ( ZI(IDNBC3+INDEXT-1).NE.0 .AND.
     +                                 ZI(IDNBC4+INDEXT-1).NE.0 ) THEN
C
C ---      CUMUL :
C          -----
           DOMMCE = DOMMCE + ZR(IDUSAE+INDEXT-1)
C
C ---      AFFECTATION DU TABLEAU A ECRIRE DANS LA TABLE :
C          ---------------------------------------------
           VALEK(IK2)  = ZK8(IDOCC2+INDEXT-1)
           VALER(4)  = ZR(IDTEM2+INDEXT-1)
           VALER(5)  = ZR(IDSNE+INDEXT-1)
           VALER(6)  = ZR(IDSPE+INDEXT-1)
           VALER(7)  = ZR(IDKEE+INDEXT-1)
           VALER(8)  = ZR(IDSATE+INDEXT-1)
           VALER(9)  = ZR(IDNADE+INDEXT-1)
           VALEI(1)  = ZI(IDNBC3+INDEXT-1)
           VALEI(2)  = ZI(IDNBC4+INDEXT-1)
           VALER(10) = ZR(IDUSAE+INDEXT-1)
           IF (VALER(10).LE.DIX*R8PREM())  VALER(10) = ZERO
           VALER(11) = DOMMCE
C
C ---      REACTUALISATION DU NOMBRE DE CYCLES SELON LE PRINCIPE :
C ---         SI NBCYCL1 < NBCYCL2  
C ---                NBCYCL2 = NBCYCL2 - NBCYCL1
C ---                NBCYCL1 = 0
C ---         SINON  
C ---                NBCYCL1 = NBCYCL1 - NBCYCL2
C ---                NBCYCL2 = 0 :
C          ----------------------
           IF ( ZI(IDNBC3+INDEXT-1) .LT. ZI(IDNBC4+INDEXT-1) ) THEN
              ZI(IDNBC4+INDEXT-1) = ZI(IDNBC4+INDEXT-1) - 
     +                                            ZI(IDNBC3+INDEXT-1)
              ZI(IDNBC3+INDEXT-1) = 0
           ELSE
              ZI(IDNBC3+INDEXT-1) = ZI(IDNBC3+INDEXT-1) - 
     +                                            ZI(IDNBC4+INDEXT-1)
              ZI(IDNBC4+INDEXT-1) = 0
           ENDIF
C
         ENDIF
C
C ---    ECRITURE DANS LA TABLE :
C        ----------------------
         IF ( NBCYCO.EQ.0 .AND. NBCYCE.EQ.0 ) GOTO 10
C
         CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALOI, VALOR, C16B, 
     +                  VALOK , 0 )
C
         CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALEI, VALER, C16B,
     +                  VALEK , 0 )
C
  10  CONTINUE
C
      CALL JEDETR ( '&&PRCCM8.INDIRO' )
      CALL JEDETR ( '&&PRCCM8.INDIRE' )
C
C --- UNITE LOGIQUE D'IMPRESSION :
C     --------------------------
      IFR = IUNIFI('RESULTAT')
C
C --- IMPRESSIONS RELATIVES AUX TRANSITOIRES TRAITES :
C     ----------------------------------------------
      DO 20 IOCC = 1 , NBFT
         CALL GETVID ( 'TRANSITOIRE', 'RESULTAT', IOCC,1,1, RESMEC, N1 )
         WRITE(IFR,1000) RESMEC
         CALL JELIRA ( JEXNUM(NCHEFF//'.LSCHEFF',IOCC), 'LONMAX',
     +                                                  NBCHEF, K8B )
         CALL JEVEUO ( JEXNUM(NCHEFF//'.NUMACCE',IOCC), 'L', JNUME )
         IF ( NBCHEF .LE. 8 ) THEN
            WRITE(IFR,1010) ( ZI(JNUME+I-1) , I=1,NBCHEF )
         ELSE
            IF ( MOD(NBCHEF,8) .EQ. 0 ) THEN
               NBLIGN = NBCHEF / 8
            ELSE
               NBLIGN = NBCHEF / 8 + 1
            ENDIF
            WRITE(IFR,1010) ( ZI(JNUME+I-1) , I=1,8 )
            DO 22 J = 2 , NBLIGN
               IF ( J*8 .LE. NBCHEF ) THEN
                  WRITE(IFR,1020) ( ZI(JNUME+I-1), I=(J-1)*8+1,J*8 )
               ELSE
                  WRITE(IFR,1020) ( ZI(JNUME+I-1), I=(J-1)*8+1,NBCHEF )
               ENDIF
 22         CONTINUE
         ENDIF
 20   CONTINUE
      WRITE(IFR,1030) 3*SM
      WRITE(IFR,1040) DOMMCO
      WRITE(IFR,1050) DOMMCE
C
 1000 FORMAT(//,' TRANSITOIRE             : ',A8)
 1010 FORMAT(   ' NUMEROS D''ORDRE TRAITES : ',8I6)
 1020 FORMAT(   '                           ',8I6)
 1030 FORMAT( /,' 3SM : ',1PE18.6)
 1040 FORMAT(   ' DOMMAGE CUMULE A L''ORIGINE   : ',1PE18.6)
 1050 FORMAT(   ' DOMMAGE CUMULE A L''EXTREMITE : ',1PE18.6)
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
