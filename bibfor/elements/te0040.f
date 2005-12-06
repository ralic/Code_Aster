      SUBROUTINE TE0040 ( OPTION, NOMTE )
      IMPLICIT NONE
       CHARACTER*16       OPTION, NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C
C     BUT:       POUR LES COQUES EN MATERIAUX COMPOSITES, CALCUL DES
C                4 CRITERES DE RUPTURE AUX NOEUDS DANS UNE COUCHE
C               
C                IL FAUT AU PREALABLE FAIRE SIGM_ELNO_DEPL 
C
C     DANS CET ORDRE :
C
C     LES 4 CRITERES  :
C       - CRIL: SUIVANT LA 1ERE DIR. D'ORTH      (= 1 VALEUR)
C       - CRIT: SUIVANT LA 2EME DIR. D'ORTH      (= 1 VALEUR)
C       - CRILT: EN CISAILLEMENT SUIVANT LT      (= 1 VALEUR)
C       - CRITH: CRITERE DE TSAI-HILL            (= 1 VALEUR)
C
C     OPTION  :  'CRIT_ELNO_RUPT'
C
C     ENTREES :  OPTION : OPTION DE CALCUL
C                NOMTE  : NOM DU TYPE ELEMENT
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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

      INTEGER      NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO
       INTEGER            ICONT,IMATE,ICRIT,JNUMCO, JGEOM
       INTEGER            INO,J,I,ICOU,NNOMAX,MATCOD
       PARAMETER         (NNOMAX = 27 )
       REAL*8             R8BID,ZERO
       REAL*8             SIGL(NNOMAX),SIGT(NNOMAX),SIGLT(NNOMAX)
       REAL*8             CRIL(NNOMAX),CRIT(NNOMAX),CRILT(NNOMAX)
       REAL*8             CRILP(NNOMAX),CRITP(NNOMAX)
       REAL*8             CRITH(NNOMAX)
       REAL*8             XT,XC,YT,YC,SLT,X,Y, ORIEN , ORIENR
       REAL*8             LIM(5),PGL(2,2),SIGM(24),VAR(2),PI,R8PI
       CHARACTER*2        CODRET(27),VAL
       CHARACTER*3        NUM
       CHARACTER*8        NOMRES(27)
       CHARACTER*10       PHENOM
C     ------------------------------------------------------------------
C
       CALL JEMARQ()
C
       ZERO = 0.D0
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)
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
       CALL JEVECH('PGEOMER','L',JGEOM)
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
 
       CALL RCVALA(MATCOD,' ','ELAS_COQMU',0,' ',R8BID,5,NOMRES,LIM,
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
       
C --- RECUPERATION DE L'ORIENTATION DE LA COUCHE
C     -------------------------------------------
       DO 12 J = 1,2
         CALL CODENT(J,'G',VAL)
         NOMRES(J) = 'C'//NUM//'_V'//VAL
   12  CONTINUE
 
       CALL RCVALA(MATCOD,' ','ELAS_COQMU',0,' ',R8BID,2,NOMRES,VAR,
     &            CODRET,'FM')
     
       ORIEN= VAR(2) 
       PI=R8PI()
       ORIENR = (ORIEN*PI)/180
 
C --- PASSAGE DES CONTRAINTES DU REPERE DE LA COQUE
C     AU REPERE LOCAL DE LA COUCHE DEFINI PAR ORIEN
C     -------------------------------------------------------
       
       PGL(1,1) = COS(ORIENR)
       PGL(2,1) =-SIN(ORIENR)
       PGL(1,2) = SIN(ORIENR)
       PGL(2,2) = COS(ORIENR)
       
       CALL DXSIRO(NNO,PGL,ZR(ICONT),SIGM)
      
C --- CALCUL DES CRITERES AUX NOEUDS :
C     -----------------------------------
       DO 10 INO = 1,NNO
         SIGL(INO)  = SIGM(1 + (INO-1)*6)
         SIGT(INO)  = SIGM(2 + (INO-1)*6)
         SIGLT(INO) = SIGM(4 + (INO-1)*6)
C

        IF (SIGL(INO).GT.ZERO) THEN 
              X=XT
        ELSE 
              X=XC
        ENDIF
        IF (SIGT(INO).GT.ZERO) THEN 
              Y=YT
        ELSE 
              Y=YC
        ENDIF

C--- PREMIER CRITERE        

        CRIL(INO) = SIGL(INO)/X
C
C--- DEUXIEME CRITERE

        CRIT(INO) = SIGT(INO)/Y
C
C--- TROISIEME CRITERE         
        CRILT(INO) = (ABS(SIGLT(INO)))/SLT
C
C--- QUATRIEME CRITERE

         CRITH(INO) =   (SIGL(INO)*SIGL(INO))/(X*X)
     &                - (SIGL(INO)*SIGT(INO))/(X*X)
     &                + (SIGT(INO)*SIGT(INO))/(Y*Y)
     &                + (SIGLT(INO)*SIGLT(INO))/(SLT*SLT) 
           
C           
10     CONTINUE
C
C ---- STOCKAGE :
C      --------
       DO 20 INO = 1,NNO
              ZR(ICRIT+(INO-1)*7) = SIGL(INO)
              ZR(ICRIT+1+(INO-1)*7) = SIGT(INO) 
              ZR(ICRIT+2+(INO-1)*7) = SIGLT(INO)
              ZR(ICRIT+3+(INO-1)*7) = CRIL(INO)
              ZR(ICRIT+4+(INO-1)*7) = CRIT(INO) 
              ZR(ICRIT+5+(INO-1)*7) = CRILT(INO)
              ZR(ICRIT+6+(INO-1)*7) = CRITH(INO)

20     CONTINUE
C
       CALL JEDEMA()
C      
       END
