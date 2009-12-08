      SUBROUTINE XAJULS(IFM,NOMA,NBMA,CNSLT,CNSLN,JCONX1,JCONX2,CLSM)
      IMPLICIT NONE
      INTEGER       IFM,NBMA,JCONX1,JCONX2,CLSM
      CHARACTER*8   NOMA
      CHARACTER*19  CNSLT,CNSLN

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/08/2006   AUTEUR MASSIN P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MASSIN P.MASSIN
C
C     ------------------------------------------------------------------
C     XFEM : REAJUSTEMENT DES LEVEL SETS (BOOK III 06/02/04)
C     -        ---            
C     BUT : ON MODIFIE LES VALEURS DE LS AUX NOEUDS SI TROP
C           PROCHES DE 0 POUR EVITER LES ERREURS D'INTEGRATION
C
C    ENTREE :
C              IFM    :   FICHIER D'IMPRESSION
C              NOMA   :   OBJET MAILLAGE
C              NBMA   :   NOMBRE DE MAILLES DU MAILLAGE
C              CNSLN  :   LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
C              CNSLT  :   LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
C       JCONX1,JCONX2 :   INDICES DE LA CONNECTIVITE
C
C    SORTIE : 
C              CNSLN  :   LEVEL-SET NORMALE
C              CNSLT  :   LEVEL-SET TANGENTE
C              CLSM   :   NOMBRE DE LEVEL SETS MODIFIEES
C
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
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER       JMA,IMA,ITYPMA,AR(12,2),NBAR,IA,NA,NB,NUNOA,NUNOB,
     &              JLNSV,JLTSV,NMAABS
      REAL*8        D,LSNA,LSNB,CRILSN,LSTA,LSTB,CRILST,R8PREM
      CHARACTER*19  MAI
      CHARACTER*8   TYPMA
      PARAMETER     (CRILSN=1.D-2, CRILST=1.D-3)

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()


      CALL JEVEUO(CNSLN//'.CNSV','E',JLNSV)
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTSV)
      
C     COMPTEUR DES LSN ET LST MODIFIÉES
      CLSM=0
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)

C     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
      DO 200 IMA=1,NBMA
        NMAABS=IMA
        ITYPMA=ZI(JMA-1+IMA)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)

C       BOUCLE SUR LES ARETES DE LA MAILLE VOLUMIQUE
        CALL CONARE(TYPMA,AR,NBAR)
        IF (NBAR .GT. 0) THEN
         DO 210 IA=1,NBAR
          NA=AR(IA,1)
          NB=AR(IA,2)
          NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
          NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)

          LSNA=ZR(JLNSV-1+(NUNOA-1)+1)
          LSNB=ZR(JLNSV-1+(NUNOB-1)+1)   

          IF (ABS(LSNA-LSNB).GT.R8PREM()) THEN
            D=LSNA/(LSNA-LSNB)
            IF (ABS(D).LE.CRILSN) THEN
C              RÉAJUSTEMENT DE LSNA
               ZR(JLNSV-1+(NUNOA-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
            IF (ABS(D-1.D0).LE.(CRILSN)) THEN
C              RÉAJUSTEMENT DE LSNB
               ZR(JLNSV-1+(NUNOB-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
          ENDIF 

          LSTA=ZR(JLTSV-1+(NUNOA-1)+1)
          LSTB=ZR(JLTSV-1+(NUNOB-1)+1)    

          IF (ABS(LSTA-LSTB).GT.R8PREM()) THEN          
            D=LSTA/(LSTA-LSTB)
            IF (ABS(D).LE.CRILST) THEN
C              RÉAJUSTEMENT DE LSTA
               ZR(JLTSV-1+(NUNOA-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
            IF (ABS(D-1.D0).LE.(CRILST)) THEN
C              RÉAJUSTEMENT DE LSTB
               ZR(JLTSV-1+(NUNOB-1)+1)=0.D0
               CLSM=CLSM+1
            ENDIF
          ENDIF
 210     CONTINUE
        ENDIF
 200  CONTINUE
 
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
