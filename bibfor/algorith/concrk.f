      SUBROUTINE CONCRK (NOMRES,PARCH,FACOBJ,NBOBJS,NOM4RK,NBSAUI,
     &                   BASEMO,MASGEN,RIGGEN,AMOGEN,NEQGEN,DT,
     &                   NBCHOC,NOECHO,INTITU,NBREDE,FONRED,NBREVI,
     &                   FONREV,METHOD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/02/2013   AUTEUR ALARCON A.ALARCON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE 'jeveux.h'
      INTEGER  PARCH,NBOBJS,NBSAUI,NEQGEN,NBCHOC,NBREDE,NBREVI,
     &         NBSAUV,LONCUM,INO,NBSTOC,NBSTO1
      INTEGER  JDEPS,JVITS,JACCS,JPASS,JORDS,JINST,JFCHO,JDCHO,JVCHO,
     &         JICHO,JREDC,JREDD,JREVC,JREVD,IBID,JDEPL,JVITE,JACCE,
     &         JDISC,JORDR,JPTEM,JFC,JDL,JVC,JIC,JVI,JVIR,JEDC,JEDD,
     &         JEVC,JEVD,DECAL1,DECAL2,DECAL3,DECAL4,DECAL5,DECAL6,
     &         IARG,NM,JREFA

      CHARACTER*4 NOM4RK,INTK,K4BID(3)
      CHARACTER*8 NOM8RK,NOMRES,INTITU(*),NOECHO(NBCHOC,*),
     &            FONRED(NBREDE,*),FONREV(NBREVI,*),NOMMAT,KREFD
      CHARACTER*(*) BASEMO,MASGEN,RIGGEN,AMOGEN
      CHARACTER*16 METHOD
      REAL*8    DT,FACOBJ
C
C    ALLOCATION DES OBJETS DEFINITIFS
C
C     GESTION DU .REFD EN CAS DE SOUS-STRUCTURATION
      IF (BASEMO.EQ.'        ') THEN
           CALL GETVID(' ','MATR_MASS',0,IARG,1,NOMMAT,NM)
           CALL JEVEUO(NOMMAT//'           .REFA','L',JREFA)
           KREFD = ZK24(JREFA-1+2)(1:8) 
      ELSE 
           KREFD = BASEMO   
      ENDIF 
C
      CALL MDALLO(NOMRES,KREFD,MASGEN,RIGGEN,AMOGEN,NEQGEN,DT,PARCH,
     &            NBCHOC,NOECHO,INTITU,NBREDE,FONRED,NBREVI,
     &            FONREV,JDEPS,JVITS,JACCS,JPASS,JORDS,JINST,
     &            JFCHO,JDCHO,JVCHO,JICHO,JREDC,JREDD,JREVC,JREVD,
     &            METHOD,IBID,K4BID,'TRAN','GLOB')
C     
C
C    BOUCLE SUR LE NOMBRE D'OBJETS VOLATILES EXISTANTS
       NBSAUV =  NBSAUI
       LONCUM = 0
       DECAL1 = 0
       DECAL2 = 0
       DECAL3 = 0
       DECAL4 = 0
       DECAL5 = 0
       DECAL6 = 0
C       
       DO 10 INO = 1,NBOBJS
C
            IF (INO.EQ.NBOBJS) THEN
                 NBSAUV = PARCH - LONCUM
            ENDIF
C
            CALL CODENT(INO,'D0',INTK)
            NOM8RK=NOM4RK//INTK

            CALL JEVEUO(NOM8RK//'           .DEPL','L',JDEPL)
            CALL JEVEUO(NOM8RK//'           .VITE','L',JVITE)
            CALL JEVEUO(NOM8RK//'           .ACCE','L',JACCE)
            CALL JEVEUO(NOM8RK//'           .DISC','L',JDISC)
            CALL JEVEUO(NOM8RK//'           .ORDR','L',JORDR)
            CALL JEVEUO(NOM8RK//'           .PTEM','L',JPTEM)     
C
            NBSTOC=NEQGEN*NBSAUV
C            
            CALL DCOPY(NBSTOC,ZR(JDEPL),1,ZR(JDEPS+DECAL1),1)
            CALL DCOPY(NBSTOC,ZR(JVITE),1,ZR(JVITS+DECAL1),1)
            CALL DCOPY(NBSTOC,ZR(JACCE),1,ZR(JACCS+DECAL1),1)
            CALL DCOPY(NBSAUV,ZR(JDISC),1,ZR(JINST+DECAL2),1)
            CALL JACOPO(NBSAUV,'I',JORDR,JORDS+DECAL2)
            CALL DCOPY(NBSAUV,ZR(JPTEM),1,ZR(JPASS+DECAL2),1)
C            
            CALL JEDETR(NOM8RK//'           .DEPL')
            CALL JEDETR(NOM8RK//'           .VITE')
            CALL JEDETR(NOM8RK//'           .ACCE')
            CALL JEDETR(NOM8RK//'           .DISC')
            CALL JEDETR(NOM8RK//'           .ORDR')
            CALL JEDETR(NOM8RK//'           .PTEM')
C                   
C     --- RECUPERATION DES CHOCS
C
            IF (NBCHOC.NE.0) THEN
               NBSTOC = 3 * NBCHOC * NBSAUV
               NBSTO1 = NBCHOC * NBSAUV
               CALL JEVEUO(NOM8RK//'           .FCHO','L',JFC)
               CALL JEVEUO(NOM8RK//'           .DLOC','L',JDL)
               CALL JEVEUO(NOM8RK//'           .VCHO','L',JVC)
               CALL JEVEUO(NOM8RK//'           .ICHO','L',JIC)
               CALL JEVEUO(NOM8RK//'           .VINT','L',JVI)
               CALL JEVEUO(NOMRES//'           .VINT','E',JVIR)
C
               CALL DCOPY(NBSTOC,ZR(JFC),1,ZR(JFCHO+DECAL3),1)
               CALL DCOPY(2*NBSTOC,ZR(JDL),1,ZR(JDCHO+2*DECAL3)
     &                    ,1)
               CALL DCOPY(NBSTOC,ZR(JVC),1,ZR(JVCHO+DECAL3),1)
               CALL JACOPO(NBSTO1,'I',JIC,JICHO+DECAL4)
               CALL DCOPY(NBSTO1,ZR(JVI),1,ZR(JVIR+DECAL4),1)
C
               CALL JEDETR(NOM8RK//'           .FCHO')
               CALL JEDETR(NOM8RK//'           .DLOC')
               CALL JEDETR(NOM8RK//'           .VCHO')
               CALL JEDETR(NOM8RK//'           .ICHO')
               CALL JEDETR(NOM8RK//'           .VINT')
            ENDIF
C                   
C     --- RECUPERATION DES RELA EFFO DEPL
C
           IF ( NBREDE.NE.0 ) THEN
               NBSTOC = NBREDE * NBSAUV
               CALL JEVEUO(NOM8RK//'           .REDC','L',JEDC)
               CALL JEVEUO(NOM8RK//'           .REDD','L',JEDD)
C               
               CALL JACOPO(NBSTOC,'I',JEDC,JREDC+DECAL5)
               CALL DCOPY(NBSTOC,ZR(JEDD),1,ZR(JREDD+DECAL5),1)
C
               CALL JEDETR(NOM8RK//'           .REDC')
               CALL JEDETR(NOM8RK//'           .REDD')
           ENDIF
C                   
C     --- RECUPERATION DES RELA EFFO VITE
C
           IF ( NBREVI.NE.0 ) THEN
               NBSTOC = NBREVI * NBSAUV
               CALL JEVEUO(NOM8RK//'           .REVC','L',JEVC)
               CALL JEVEUO(NOM8RK//'           .REVD','L',JEVD)
C
               CALL JACOPO(NBSTOC,'I',JEVC,JREVC+DECAL6)
               CALL DCOPY(NBSTOC,ZR(JEVD),1,ZR(JREVD+DECAL6),1)
C
               CALL JEDETR(NOM8RK//'           .REVC')
               CALL JEDETR(NOM8RK//'           .REVD')
           ENDIF
C          LONGUEUR CUMULEE DES OBJETS COPIES (EN MULTIPLE DE NBSAUV)
           LONCUM = LONCUM + NBSAUV 
C          NBSAUV DU PROCHAIN OBJET ET DECALAGES  
           DECAL1=DECAL1+NBSTOC
           DECAL2=DECAL2+NBSAUV
           DECAL3=DECAL3+(3*NBCHOC*NBSAUV)
           DECAL4=DECAL4+(NBCHOC*NBSAUV)
           DECAL5=DECAL5+(NBREDE*NBSAUV)
           DECAL6=DECAL6+(NBREVI*NBSAUV)
C          PROCHAIN NBSAUV 
           NBSAUV = INT(NBSAUV*FACOBJ)
C
 10   CONTINUE    
           CALL JEDETC('V','&&RK',1)
C
      END
