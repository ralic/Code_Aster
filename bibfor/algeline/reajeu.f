      SUBROUTINE REAJEU(RESOCO, DEPTOT, DEPDEL, LMAT)
C
      IMPLICIT      NONE
      INTEGER       LMAT
      CHARACTER*24  RESOCO, DEPTOT, DEPDEL
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/02/2002   AUTEUR ADBHHVV V.CANO 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C ======================================================================
C --- BUT : RECUPERATION DU JEU ----------------------------------------
C ======================================================================
C IN/OUT : RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT --------------
C IN --- : DEPTOT : DEPLACEMENTS TOTALS DEPUIS L'INSTANT INITIAL -------
C IN --- : DEPDEL : DEPLACEMENTS DEPUIS LE DERNIER PAS DE TEMPS CONVERGE
C IN --- : LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE ---
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      INTEGER      II, NBLIAI, JDECAL, NBDDL, NEQ, JDEPDE
      INTEGER      JAPJEU, JJEUIN, JAPPTR, JAPPAR, JAPCOE, JAPDDL, JDEPP
      REAL*8       VAL
      CHARACTER*24 APJEU, JEUINI, APPOIN, APPARI, APCOEF, APDDL
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES VECTEURS DE JEU ---------------------------------
C ======================================================================
      APJEU  = RESOCO (1:14)//'.APJEU'
      JEUINI = RESOCO (1:14)//'.JEUINI'
      APPOIN = RESOCO (1:14)//'.APPOIN'
      APPARI = RESOCO (1:14)//'.APPARI'
      APCOEF = RESOCO (1:14)//'.APCOEF'
      APDDL  = RESOCO (1:14)//'.APDDL'
      CALL     JEVEUO (APJEU ,'E',JAPJEU)
      CALL     JEVEUO (JEUINI,'L',JJEUIN)
      CALL     JEVEUO (APPOIN,'L',JAPPTR)
      CALL     JEVEUO (APPARI,'L',JAPPAR)
      CALL     JEVEUO (APCOEF,'L',JAPCOE)
      CALL     JEVEUO (APDDL ,'L',JAPDDL)
      CALL     JEVEUO (DEPTOT(1:19)//'.VALE','L',JDEPP)
      CALL     JEVEUO (DEPDEL(1:19)//'.VALE','L',JDEPDE)
      NBLIAI = ZI(JAPPAR)
      NEQ    = ZI(LMAT+2)
      DO 10 II = 1,NBLIAI
         JDECAL = ZI(JAPPTR+II-1)
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
         CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     +               ZR(JDEPP),VAL)
         ZR(JAPJEU+II-1) = ZR(JJEUIN+II-1) - VAL
 10   CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
