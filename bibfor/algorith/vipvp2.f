      SUBROUTINE VIPVP2(NBVARI,VINTM,VINTP,ADVICO,VICPVP,PVP0,
     +         PVP1,P2,DP2,T,DT,KH,MAMOLV,R,RHO11M,YATE,PVP,PVPM,RETCOM)
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'

      INTEGER       NBVARI,ADVICO,VICPVP,YATE,RETCOM
      REAL*8        VINTM(NBVARI),VINTP(NBVARI),PVP0,PVP1,P2,DP2,T,DT
      REAL*8        MAMOLV,R,RHO11M,PVP,PVPM,KH
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C --- CALCUL ET STOCKAGE DE LA PRESSION DE VAPEUR DANS LE CAS ----------
C --- AVEC AIR DISSOUS -------------------------------------------------
C ======================================================================
      INTEGER       IADZI,IAZK24,UMESS,IUNIFI
      REAL*8        VARBIO,R8PREM
      CHARACTER*8   NOMAIL
C ======================================================================
C ======================================================================
      VARBIO = (RHO11M*KH/PVP1)-MAMOLV*(1+R*LOG(T/(T-DT)))
C ======================================================================
C --- VERIFICATION DES COHERENCES --------------------------------------
C ======================================================================
      IF (ABS(VARBIO).LT.R8PREM()) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'VIPVP2','DIVISION PAR ZERO A LA MAILLE',
     +                                                            NOMAIL
         RETCOM = 1
         GO TO 30
      ENDIF
      PVPM = VINTM(ADVICO+VICPVP) + PVP0
      PVP  = (RHO11M*KH-MAMOLV*(PVPM+(P2-DP2)*R*LOG(T/(T-DT))))/VARBIO
      IF ((P2-PVP).LT.0.D0) THEN
         UMESS  = IUNIFI('MESSAGE')
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL = ZK24(IAZK24-1+3) (1:8)
         WRITE (UMESS,9001) 'VIPVP2','PGAZ-PVAP <=0 A LA MAILLE: ',
     +                                                            NOMAIL
         RETCOM = 1
         GO TO 30
      ENDIF
      VINTP(ADVICO+VICPVP) = PVP - PVP0
C ======================================================================
 30   CONTINUE
C =====================================================================
 9001 FORMAT (A8,2X,A30,2X,A8)
C ======================================================================
      END
