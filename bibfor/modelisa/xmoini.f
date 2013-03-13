      SUBROUTINE XMOINI(NH8,NH20,NP6,NP15,NP5,NP13,NT4,NT10,NCPQ4,NCPQ8,
     &                  NCPT3,NCPT6,NDPQ4,NDPQ8,NDPT3,NDPT6,NF4,NF8,NF3,
     &                  NF6,NPF2,NPF3,NAXT3,NAXQ4,NAXQ8,NAXT6,NAX2,NAX3,
     &                  NTH8,NTP6,NTP5,NTT4,NTPQ4,NTPT3,NTAQ4,NTAT3)

      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      INTEGER       NH8(14),NH20(7),NP6(14),NP15(7),NP5(14),NP13(7)
      INTEGER       NT4(14),NT10(7)
      INTEGER       NCPQ4(14),NCPQ8(7),NCPT3(14),NCPT6(7), NDPQ4(14)
      INTEGER       NDPQ8(7),NDPT3(14),NDPT6(7),NF4(11),NF8(7),NF3(11)
      INTEGER       NF6(7),NPF2(11),NPF3(7)
      INTEGER       NAXT3(7),NAXQ4(7),NAXQ8(7),NAXT6(7),NAX2(7),NAX3(7)
      INTEGER       NTH8(7),NTP6(7),NTP5(7),NTT4(7),NTPQ4(7),NTPT3(7)
      INTEGER       NTAQ4(7),NTAT3(7)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 11/03/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
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
C RESPONSABLE GENIAUT
C TOLE CRP_21
C
C
C ----------------------------------------------------------------------
C
C ROUTINE XFEM APPELEE PAR MODI_MODELE_XFEM (OP0113)
C
C    BUT : INITIALISER LES COMPTEURS DES NOMBRES D'ELEMENTS
C
C ----------------------------------------------------------------------
C
C
C
C
      INTEGER    I
C
      CALL JEMARQ()

      DO 10 I=1,7
        NH8(I)=0
        NH20(I)=0
        NP6(I)=0
        NP15(I)=0
        NP5(I)=0
        NP13(I)=0
        NT4(I)=0
        NT10(I)=0
        NCPQ4(I)=0
        NCPQ8(I)=0
        NCPT3(I)=0
        NCPT6(I)=0
        NDPQ4(I)=0
        NDPQ8(I)=0
        NDPT3(I)=0
        NDPT6(I)=0
        NF4(I)=0
        NF8(I)=0
        NF3(I)=0
        NF6(I)=0
        NPF2(I)=0
        NPF3(I)=0
        NAXT3(I)=0
        NAXQ4(I)=0
        NAXQ8(I)=0
        NAXT6(I)=0
        NAX2(I)=0
        NAX3(I)=0
        NAXT3(I)=0
        NTH8(I)=0
        NTP6(I)=0
        NTP5(I)=0
        NTT4(I)=0
        NTPQ4(I)=0
        NTPT3(I)=0
        NTAQ4(I)=0
        NTAT3(I)=0
 10   CONTINUE
      DO 20 I=8,11
        NH8(I)=0
        NP6(I)=0
        NP5(I)=0
        NT4(I)=0
        NCPT3(I)=0
        NCPQ4(I)=0
        NDPQ4(I)=0
        NDPT3(I)=0
        NF4(I)=0
        NF3(I)=0
        NPF2(I)=0
 20   CONTINUE
      DO 30 I=12,14
        NH8(I)=0
        NP6(I)=0
        NP5(I)=0
        NT4(I)=0
        NCPT3(I)=0
        NCPQ4(I)=0
        NDPQ4(I)=0
        NDPT3(I)=0
 30   CONTINUE
      

      CALL JEDEMA()
      END
