      SUBROUTINE FORTAN(FN,XLOCAL,VITLOC,CFROTT,KTANG,CTANG,IADHER,
     &                  OLDVT,OLDFT,OLDXLO,COST,SINT,FTANGE,FLOCAL,VT)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/01/2001   AUTEUR CIBHHPD D.NUNEZ 
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
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DE LA FORCE TANGENTIELLE DE CONTACT
C -----------
C               APPELANTS : CALREF, CALRES
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      REAL*8     FN, XLOCAL(*), VITLOC(*), CFROTT, KTANG, CTANG
      INTEGER    IADHER
      REAL*8     OLDVT(*), OLDFT(*), OLDXLO(*), COST, SINT,
     &           FTANGE(*), FLOCAL(*), VT(*)
C
C VARIABLES LOCALES
C -----------------
      REAL*8     XSCAL, XNORVT, DXT(3), XNFTAN
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  SQRT
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C-----------------------------------------------------------------------
C 1.  CALCUL DE LA VITESSE TANGENTIELLE
C-----------------------------------------------------------------------
C
      VT(1)  = -SINT*VITLOC(2) + COST*VITLOC(3)
      VT(2)  = VITLOC(1)
      XNORVT = SQRT(VT(1)*VT(1)+VT(2)*VT(2))
C
C ... PRODUIT SCALAIRE DE LA VITESSE TANGENTIELLE A L'INSTANT COURANT
C ... PAR LA VITESSE TANGENTIELLE A L'INSTANT PRECEDENT POUR DETECTER
C ... UN EVENTUEL CHANGEMENT DE SIGNE
C
      XSCAL = VT(1)*OLDVT(1) + VT(2)*OLDVT(2)
C
C-----------------------------------------------------------------------
C 2.  ESTIMATION DE LA FORCE TANGENTIELLE DE FROTTEMENT
C-----------------------------------------------------------------------
C 2.1 CAS DU GLISSEMENT
C     -----------------
      IF ( (XSCAL.GE.0.0D0).AND.(IADHER.EQ.0).AND.(XNORVT.GT.1.0D-06) )
     &   THEN
C
         FTANGE(1) = -CFROTT * FN * VT(1) / XNORVT
         FTANGE(2) = -CFROTT * FN * VT(2) / XNORVT
C
         OLDFT(1)  = FTANGE(1)
         OLDFT(2)  = FTANGE(2)
         OLDXLO(1) = XLOCAL(1)
         OLDXLO(2) = XLOCAL(2)
         OLDXLO(3) = XLOCAL(3)
C
C 2.2 CAS DE L'ADHERENCE
C     ------------------
      ELSE
C
C ...... DISTANCE DE GLISSEMENT
C
         DXT(1)=(-(XLOCAL(2)-OLDXLO(2))*SINT+(XLOCAL(3)-OLDXLO(3))*COST)
         DXT(2)= XLOCAL(1)-OLDXLO(1)
C
         IADHER = 1
C
         FTANGE(1) = OLDFT(1) - KTANG * DXT(1) - CTANG * VT(1)
         FTANGE(2) = OLDFT(2) - KTANG * DXT(2) - CTANG * VT(2)
C
         XNFTAN = SQRT(FTANGE(1)*FTANGE(1)+FTANGE(2)*FTANGE(2))
         IF ( XNFTAN.GT.(CFROTT*FN) ) THEN
            IADHER = 0
            IF ( XNORVT.EQ.0.0D0 ) THEN
               FTANGE(1) = 0.0D0
               FTANGE(2) = 0.0D0
            ELSE
               FTANGE(1) = -CFROTT * FN * VT(1) / XNORVT
               FTANGE(2) = -CFROTT * FN * VT(2) / XNORVT
            ENDIF
          ENDIF
            OLDFT(1)  = FTANGE(1)
            OLDFT(2)  = FTANGE(2)
            OLDXLO(1) = XLOCAL(1)
            OLDXLO(2) = XLOCAL(2)
            OLDXLO(3) = XLOCAL(3)
         ENDIF
C
C
C 2.3 MISE A JOUR DE OLDVT POUR L'INSTANT SUIVANT
C     -------------------------------------------
      OLDVT(1) = VT(1)
      OLDVT(2) = VT(2)
C
C-----------------------------------------------------------------------
C 3.  ACCUMULATION DE LA FORCE TANGENTIELLE ET DE LA FORCE NORMALE
C     DE FROTTEMENT (REPERE LOCAL)
C-----------------------------------------------------------------------
C
      FLOCAL(1) = FLOCAL(1) + FTANGE(2)
      FLOCAL(2) = FLOCAL(2) - FTANGE(1)*SINT
      FLOCAL(3) = FLOCAL(3) + FTANGE(1)*COST
C
C --- FIN DE FORTAN.
      END
