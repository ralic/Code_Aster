      SUBROUTINE TE0040(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/06/2002   AUTEUR CIBHHAB S.VANDENBERGHE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
C
C     BUT:       POUR LES COQUES EN MATERIAUX COMPOSITES, CALCUL DES
C                6 CRITERES DE RUPTURE AUX NOEUDS DANS UNE COUCHE
C               
C                IL FAUT AU PREALABLE FAIRE SIGM_ELNO_DEPL 
C
C     DANS CET ORDRE :
C
C     LES 6 CRITERES  :
C       - CRIL: SUIVANT L EN TRACTION (1ERE DIR. D'ORTH ) (= 1 VALEUR)
C       - CRILP: SUIVANT L EN COMPRESSION                 (= 1 VALEUR)
C       - CRIT: SUIVANT T EN TRACTION (2EME DIR. D'ORTH ) (= 1 VALEUR)
C       - CRITP: SUIVANT T EN TRACTION                    (= 1 VALEUR)
C       - CRILT: EN CISAILLEMENT SUIVANT LT              (= 1 VALEUR)
C       - CRITH: CRITERE DE TSAI-HILL                    (= 1 VALEUR)
C
C     OPTION  :  'CRIT_ELNO_RUPT'
C
C     ENTREES :  OPTION : OPTION DE CALCUL
C                NOMTE  : NOM DU TYPE ELEMENT

       INTEGER            ICONT,IMATE,ICRIT,JNUMCO
       INTEGER            JIN,INO,J,I,ICOU,I1
       INTEGER            NNO,NNOMAX,MATCOD ,IBID 
       PARAMETER         (NNOMAX = 27 )
       REAL*8             R8BID,ZERO
       REAL*8             SIGL,SIGT,SIGLT
       REAL*8             CRIL(NNOMAX),CRIT(NNOMAX),CRILT(NNOMAX)
       REAL*8             CRILP(NNOMAX),CRITP(NNOMAX)
       REAL*8             CRITH(NNOMAX)
       REAL*8             XT,XC,YT,YC,SLT
       REAL*8             LIM(5)
       CHARACTER*2        CODRET(27),VAL
       CHARACTER*3        NUM
       CHARACTER*8        ELREFE,NOMRES(27)
       CHARACTER*10       PHENOM
       CHARACTER*16       NOMTE,OPTION
       CHARACTER*24       CHCTE

C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
       CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
       INTEGER            ZI
       COMMON  / IVARJE / ZI(1)
       REAL*8             ZR
       COMMON  / RVARJE / ZR(1)
       COMPLEX*16         ZC
       COMMON  / CVARJE / ZC(1)
       LOGICAL            ZL
       COMMON  / LVARJE / ZL(1)
       CHARACTER*8        ZK8
       CHARACTER*16                ZK16
       CHARACTER*24                          ZK24
       CHARACTER*32                                    ZK32
       CHARACTER*80                                              ZK80
       COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
       ZERO = 0.D0
C      
       CALL ELREF1(ELREFE)
C
       CALL JEMARQ()
C
       CHCTE = '&INEL.'//ELREFE//'.DESI'
       CALL JEVETE(CHCTE,'L',JIN)
       NNO   = ZI(JIN)
C
C --- RECUPERATION DU CHAMP DE CONTRAINTES AUX NOEUDS DE L'ELEMENT
C --- POUR LEQUEL ON VA CALCULER LES SIX CRITERES DE RUPTURE .
C --- CE CHAMP DE CONTRAINTES EST ISSU D'UN CALCUL PAR CALC_ELEM
C --- AVEC L'OPTION 'SIGM_ELNO_DEPL' POUR L'ELASTICITE
C     -------------------------------------------------
       CALL JEVECH('PCONTRR','L',ICONT)
       CALL JEVECH('PMATERC','L',IMATE)
       CALL JEVECH('PCRITER','E',ICRIT)
       CALL JEVECH('PNUMCOR','L',JNUMCO)
       ICOU=ZI(JNUMCO)
       MATCOD = ZI(IMATE)

C --- RECUPERATION DES VALEURS LIMITES DE CONTRAINTE
C     ----------------------------------------------

       CALL RCCOMA(MATCOD,'ELAS',PHENOM,CODRET)
       
       CALL CODENT(ICOU,'G',NUM)
       DO 11 J = 1,5
         I=J+78
         CALL CODENT(I,'G',VAL)
         NOMRES(J) = 'C'//NUM//'_V'//VAL
   11  CONTINUE
 
       CALL RCVALA(MATCOD,'ELAS_COQMU',0,' ',R8BID,5,NOMRES,LIM,
     &            CODRET,'FM')

C     LIMITE EN TRACTION SUIVANT L 
       XT=LIM(1)    
      
C     LIMITE EN COMPRESSION SUIVANT L     
       XC=LIM(2)
      
C     LIMITE EN TRACTION SUIVANT T     
       YT=LIM(3)
      
C     LIMITE EN COMPRESSION SUIVANT T     
       YC=LIM(4)
      
C     LIMITE EN CISAILLEMENT SUIVANT LT 
       SLT=LIM(5)    
                       
      
C --- CALCUL DES CRITERES AUX NOEUDS :
C     -----------------------------------
       DO 10 INO = 1,NNO
         SIGL  = ZR(ICONT + (INO-1)*6)
         SIGT  = ZR(ICONT +1 + (INO-1)*6)
         SIGLT = ZR(ICONT +3 + (INO-1)*6)
C 
C--- PREMIER CRITERE        
         IF (SIGL.GT.ZERO) THEN
            CRIL(INO) = SIGL/XT
            CRILP(INO) = ZERO
         ELSE
            CRIL(INO) = ZERO
            CRILP(INO) = SIGL/XC
         ENDIF
C
C--- DEUXIEME CRITERE
         IF (SIGT.GT.ZERO) THEN
            CRIT(INO) = SIGT/YT
            CRITP(INO) = ZERO
         ELSE
            CRIT(INO) = ZERO
            CRITP(INO) = SIGT/YC
         ENDIF
C
C--- TROISIEME CRITERE         
         CRILT(INO) = SIGLT/SLT
C
C--- QUATRIEME CRITERE
         CRITH(INO) =   (SIGL*SIGL)/(XT*XT)
     &                - (SIGL*SIGT)/(XT*XT)
     &                + (SIGT*SIGT)/(YT*YT)
     &                + (SIGLT*SIGLT)/(SLT*SLT)             
C           
10     CONTINUE
C
C ---- STOCKAGE :
C      --------
       DO 20 INO = 1,NNO
              ZR(ICRIT+(INO-1)*6) = CRIL(INO)
              ZR(ICRIT+1+(INO-1)*6) = CRILP(INO) 
              ZR(ICRIT+2+(INO-1)*6) = CRIT(INO)
              ZR(ICRIT+3+(INO-1)*6) = CRITP(INO)
              ZR(ICRIT+4+(INO-1)*6) = CRILT(INO)
              ZR(ICRIT+5+(INO-1)*6) = CRITH(INO)
20     CONTINUE
C
       CALL JEDEMA()
C      
       END
