      SUBROUTINE FTANG( FN,XLOCAL,VITLOC,CFROTD,CFROTS, KTANG,CTANG,
     &               IADHER,OLDVT,OLDFT,OLDXLO,COST,SINT,FTANGE,FLOCAL,
     &                VT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C***********************************************************************
C 01/01/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C     FONCTION  : CALCULE LA FORCE TANGENTIELLE DE CHOC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C        NOM        MODE                    ROLE
C  ________________ ____ ______________________________________________
C                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
C  ________________ ____ ______________________________________________
C    FN             <--   FORCE NORMALE
C    XLOCAL         <--   POSITION DANS LE REPERE LOCAL
C    VITLOC         <--   VITESSE DANS LE REPERE LOCAL
C    CFROTD         <--   COEFFICIENT DE FROTTEMENT DYNAMIQUE
C    CFROTS         <--   COEFFICIENT DE FROTTEMENT STATIQUE
C    KTANG          <--   RAIDEUR TANGENTIEL DE CHOC
C    CTANG          <--   AMORTISSEUR TANGENTIEL DE CHOC
C    IADHER         <-->  INDICATEUR DE GLISSEMENT =0 GLISSEMENT
C    OLDVT          <-->  VITESSE DE GLISSEMENT AU PAS PRECEDENT
C    OLDFT          <-->  FORCE TANGENTIELLE DE DEBUT DE GLISSEMENT
C    OLDXLO         <-->  POSITION DE DEBUT DE GLISSEMENT
C    COST,SINT      <--   DIRECTION NORMALE A L'OBSTACLE
C    FTANGE          -->  FORCE TANGENTIELLE DE CHOC (MODULE)
C    FLOCAL          -->  FORCE TANGENTIELLE DE CHOC REP. LOCAL
C    VT              -->  VITESSE TANGENTIELLE DE CHOC REP. LOCAL
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IADHER
      REAL*8 VITLOC(3),FLOCAL(3),XLOCAL(3),OLDXLO(3),VT(2)
      REAL*8 FN,FTANGE(2),KTANG,CTANG,DXT(2)
      REAL*8 COST,SINT,OLDFT(2),OLDVT(2),CFROTS,CFROTD
C
C     CALCUL DE LA VITESSE TANGENTIELLE
C
C-----------------------------------------------------------------------
      REAL*8 XNFTAN ,XNORVT ,XSCAL 
C-----------------------------------------------------------------------
      VT(1) = -SINT*VITLOC(2) + COST*VITLOC(3)
      VT(2) = VITLOC(1)
C
C     TESTE CHANGEMENT DE SIGNE VITESSE TANGENTIELLE
C
      XSCAL=VT(1)*OLDVT(1)+VT(2)*OLDVT(2)
      XNORVT=SQRT(VT(1)**2+VT(2)**2)
      IF (((XSCAL).GE.0.D0).AND.(IADHER.EQ.0).AND.
     &  (XNORVT.GT.1.D-6))  THEN
C
C       CAS DU GLISSEMENT
C
C       FORCE DE FROTTEMENT
C
        FTANGE(1)=-CFROTD*FN*VT(1)/XNORVT
        FTANGE(2)=-CFROTD*FN*VT(2)/XNORVT
        OLDFT(1)=FTANGE(1)
        OLDFT(2)=FTANGE(2)
        OLDXLO(1)=XLOCAL(1)
        OLDXLO(2)=XLOCAL(2)
        OLDXLO(3)=XLOCAL(3)
      ELSE
C
C       CAS DE L'ADHERENCE
C
C       DISTANCE DE GLISSEMENT
C
        DXT(1)=(-(XLOCAL(2)-OLDXLO(2))*SINT+(XLOCAL(3)-OLDXLO(3))*COST)
        DXT(2)= XLOCAL(1)-OLDXLO(1)
C
C       FORCE DE FROTTEMENT
C
        IADHER = 1
        FTANGE(1) = OLDFT(1) - KTANG * DXT(1) - CTANG * VT(1)
        FTANGE(2) = OLDFT(2) - KTANG * DXT(2) - CTANG * VT(2)
        XNFTAN=SQRT(FTANGE(1)**2+FTANGE(2)**2)
        IF (XNFTAN.GT.(CFROTS*FN)) THEN
          IADHER = 0
          IF(XNORVT.EQ.0.D0)THEN
            FTANGE(1)=0.0D0
            FTANGE(2)=0.0D0
          ELSE
            FTANGE(1)=-CFROTD*FN*VT(1)/XNORVT
            FTANGE(2)=-CFROTD*FN*VT(2)/XNORVT
          ENDIF
          OLDFT(1)=FTANGE(1)
          OLDFT(2)=FTANGE(2)
          OLDXLO(1)=XLOCAL(1)
          OLDXLO(2)=XLOCAL(2)
          OLDXLO(3)=XLOCAL(3)
        ENDIF
      ENDIF
      OLDVT(1) = VT(1)
      OLDVT(2) = VT(2)
C
C     CALCUL DE LA FORCE DANS LE REPERE LOCAL
C
      FLOCAL(1)=FLOCAL(1)+FTANGE(2)
      FLOCAL(2)=FLOCAL(2)-FTANGE(1)*SINT
      FLOCAL(3)=FLOCAL(3)+FTANGE(1)*COST
      END
