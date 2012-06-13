      SUBROUTINE JNI015(ELREFE,NMAXOB,LIOBJ,NBOBJ)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE VABHHTS J.PELLET
      INCLUDE 'jeveux.h'
      CHARACTER*8 ELREFE
      INTEGER NMAXOB,NBOBJ
      CHARACTER*24 LIOBJ(NMAXOB)

C BUT :  INITIALISATION DES ELEMENTS HOMOGENEISES
C.......................................................................
      CHARACTER*24  CARAC,FF
      CHARACTER*6   FLUI,POU,GEOM,ALIAS
      PARAMETER     (FLUI = 'FLUIDE',POU='POUTRE',GEOM='HEXA8 ')
      INTEGER       NPG1(2,2),NPG2(2,2),LCARAC,NNO1,NNO2,IRET,ICARAC,
     &              NPG,LFF,LFFT,N,NPGI(3)
      INTEGER       IFF,IFFT,IPOIDS,IVF1,IVF12,IVF2,IDPDX1,IDPDY1,
     &              IDPDZ1,IDSDX1,IDSDY1,IDSDZ1,IDSXY1,IDSXZ1,IDSYZ1,
     &              ICOPG,I,J,IDER,IDPDX2,IDPDY2,IDPDZ2,
     &              IDSDX2,IDSDY2,IDSDZ2,IDSXY2,IDSXZ2,IDSYZ2
      REAL*8        XIN(20),YIN(20),ZIN(20),XG,YG,ZG,BID(1)
C
C DEB ------------------------------------------------------------------


      CARAC='&INEL.'//ELREFE//'.CARAC'
      FF   ='&INEL.'//ELREFE//'.FF'


      NBOBJ = 2
      CALL ASSERT(NMAXOB.GT.NBOBJ)
      LIOBJ(1) = CARAC
      LIOBJ(2) = FF


      LCARAC = 10
C
      IF(ELREFE.EQ.'POHOH20') THEN
C       1 ERE FAMILLE (DX, DRY)
C       2 EME FAMILLE (PRES)
C       --- NOMBRE DE NOEUDS
        NNO1  = 8
        NNO2  = 20
        ALIAS = 'HEXA20'
C       --- NOMBRE DE POINTS DE GAUSS ASSOCIES A LA PREMIERE FAMILLE
C       --->       MATRICE DE RAIDEUR : INTEGRATION SELON X ET Y
        NPG1(1,1) = 2
C       --->       MATRICE DE RAIDEUR : INTEGRATION SELON Z
        NPG1(1,2) = 2
C       --->       MATRICE DE MASSE : INTEGRATION SELON X ET Y
        NPG1(2,1) = 2
C       --->       MATRICE DE MASSE : INTEGRATION SELON Z
        NPG1(2,2) = 4
C       --- NOMBRE DE POINTS DE GAUSS ASSOCIES A LA DEUXIEME FAMILLE
C       ---        MATRICE DE MASSE : CONTRIBUTION DE P (MATRICE A)
C       --->       INTEGRATION SELON X ET Y
        NPG2(1,1) = 3
C       --->       INTEGRATION SELON Z
        NPG2(1,2) = 3
C       ---        MATRICE DE MASSE : COUPLAGE FLUIDE-STRUCTURE
C       --->       INTEGRATION SELON X ET Y
        NPG2(2,1) = 2
C       --->       INTEGRATION SELON Z
        NPG2(2,2) = 3
C
      ELSE IF(ELREFE.EQ.'POHOH8') THEN
        NNO1 = 8
        NNO2 = 8
        ALIAS = 'HEXA8'
        NPG1(1,1) = 2
        NPG1(1,2) = 2
        NPG1(2,1) = 2
        NPG1(2,2) = 4
        NPG2(1,1) = 2
        NPG2(1,2) = 2
        NPG2(2,1) = 2
        NPG2(2,2) = 3
C
      ELSE
        CALL U2MESS('F','ELEMENTS2_29')
      ENDIF
C
      CALL JEEXIN(CARAC,IRET)
      IF(IRET.NE.0) GOTO 9998

      CALL WKVECT(CARAC,'V V I',LCARAC,ICARAC)
      ZI(ICARAC  ) = NNO1
      ZI(ICARAC+1) = NNO2
      N = 1
      DO 1 I = 1, 2
        DO 1 J = 1, 2
          N = N+1
          ZI(ICARAC+N)= NPG1(I,J)
          ZI(ICARAC+N+4) = NPG2(I,J)
 1    CONTINUE
C --- PLACE MEMOIRE POUR VALEURS DES FONCTIONS DE FORME ET DE LEURS ---
C ---              DERIVEES AU DIFFERENTS POINTS DE GAUSS           ---
      NPG = NPG1(1,1)*NPG1(1,1)*NPG1(1,2)
      LFF = NPG + 10 * ( NPG * 2*NNO1 ) + 10 * NPG * NNO1
      NPG = NPG1(2,1)*NPG1(2,1)*NPG1(2,2)
      LFF = LFF + NPG + NPG * 2*NNO1 + 4 * NPG * NNO1
      NPG = NPG2(1,1)*NPG2(1,1)*NPG2(1,2)
      LFF = LFF + NPG + 4 * NPG * NNO2 + 4 * NPG * NNO1
      NPG = NPG2(2,1)*NPG2(2,1)*NPG2(2,2)
      LFF = LFF + NPG + NPG * 2*NNO1 + 4 * NPG * NNO2 + 4 * NPG * NNO1
C
      CALL WKVECT(FF   ,'V V R',LFF,IFF)
C --- PLACE MEMOIRE POUR COORDONNEES DES DIFFERENTS POINTS DE GAUSS ---
      LFFT = 3*( NPG1(1,1)*NPG1(1,1)*NPG1(1,2) +
     &           NPG1(2,1)*NPG1(2,1)*NPG1(2,2) +
     &           NPG2(1,1)*NPG2(1,1)*NPG2(1,2) +
     &           NPG2(2,1)*NPG2(2,1)*NPG2(2,2) )
      CALL WKVECT('&&JNI014.FFT','V V R',LFFT,IFFT)
C     --------------------------------------------
C     --- COORDONNEES INTRINSEQUES DE L'HEXA8 ----
C     --------------------------------------------
      DO 500 I = 1, 4
        ZIN(I)   = -1.D0
        ZIN(4+I) =  1.D0
 500  CONTINUE
      DO 510 I = 1, 2
        XIN(1+4*(I-1)) = -1.D0
        XIN(2+4*(I-1)) =  1.D0
        XIN(3+4*(I-1)) =  1.D0
        XIN(4+4*(I-1)) = -1.D0
 510  CONTINUE
      DO 520 I = 1, 2
        YIN(1+4*(I-1)) = -1.D0
        YIN(2+4*(I-1)) = -1.D0
        YIN(3+4*(I-1)) =  1.D0
        YIN(4+4*(I-1)) =  1.D0
 520  CONTINUE
C     --------------------------------------------
C     --- COORDONNEES INTRINSEQUES DE L'EXA20 ----
C     --------------------------------------------
      DO 530 I = 9, 12
        ZIN(I)   = -1.D0
        ZIN(4+I) = 0.D0
        ZIN(8+I) = 1.D0
 530  CONTINUE
        XIN(9)   = 0.D0
        XIN(10)  = 1.D0
        XIN(11)  = 0.D0
        XIN(12)  = -1.D0
        XIN(13)  = -1.D0
        XIN(14)  = 1.D0
        XIN(15)  = 1.D0
        XIN(16)  = -1.D0
        XIN(17)  = 0.D0
        XIN(18)  = 1.D0
        XIN(19)  = 0.D0
        XIN(20)  = -1.D0
        YIN(9)   = -1.D0
        YIN(10)  = 0.D0
        YIN(11)  = 1.D0
        YIN(12)  = 0.D0
        YIN(13)  = -1.D0
        YIN(14)  = -1.D0
        YIN(15)  = 1.D0
        YIN(16)  = 1.D0
        YIN(17)  = -1.D0
        YIN(18)  = 0.D0
        YIN(19)  = 1.D0
        YIN(20)  = 0.D0
C     --------------------------------------------
C     --- PREMIERE FAMILLE DE POINTS DE GAUSS ----
C     --- MATRICE DE RAIDEUR : POUTRE         ----
C     --------------------------------------------
      NPG = NPG1(1,1) * NPG1(1,1) * NPG1(1,2)
      IPOIDS = IFF
      IVF1   = IPOIDS + NPG
      IDPDX1 = IVF1   + NPG * 2 * NNO1
      IDPDY1 = IDPDX1 + NPG * 2 * NNO1
      IDPDZ1 = IDPDY1 + NPG * 2 * NNO1
      IDSDX1 = IDPDZ1 + NPG * 2 * NNO1
      IDSDY1 = IDSDX1 + NPG * 2 * NNO1
      IDSDZ1 = IDSDY1 + NPG * 2 * NNO1
      IDSXY1 = IDSDZ1 + NPG * 2 * NNO1
      IDSXZ1 = IDSXY1 + NPG * 2 * NNO1
      IDSYZ1 = IDSXZ1 + NPG * 2 * NNO1
      IVF2   = IDSYZ1 + NPG * 2 * NNO1
      IDPDX2 = IVF2   + NPG * NNO1
      IDPDY2 = IDPDX2 + NPG * NNO1
      IDPDZ2 = IDPDY2 + NPG * NNO1
      IDSDX2 = IDPDZ2 + NPG * NNO1
      IDSDY2 = IDSDX2 + NPG * NNO1
      IDSDZ2 = IDSDY2 + NPG * NNO1
      IDSXY2 = IDSDZ2 + NPG * NNO1
      IDSXZ2 = IDSXY2 + NPG * NNO1
      IDSYZ2 = IDSXZ2 + NPG * NNO1
      ICOPG  = IFFT
C --- POIDS ET COORDONNEES DES POINTS DE GAUSS
      NPGI(1) = NPG1(1,1)
      NPGI(2) = NPG1(1,1)
      NPGI(3) = NPG1(1,2)
      CALL GAUSCH ( NPGI, ZR(ICOPG), ZR(ICOPG+NPG),
     &                ZR(ICOPG+NPG+NPG), ZR(IPOIDS) )
C --- FONCTIONS DE FORME ET DERIVEES PREMIERES ET SECONDES
C                 AU POINT DE GAUSS
      DO 100 I = 1, NPG
        XG = ZR(ICOPG+I-1)
        YG = ZR(ICOPG+I-1+NPG)
        ZG = ZR(ICOPG+I-1+NPG+NPG)
C --- POUR LES POUTRES
        IDER = 2
        CALL CLFFCH ( ALIAS, POU, NNO1, XG, YG, ZG,
     &       XIN, YIN, ZIN,
     &       ZR(IVF1  +(I-1)*2*NNO1), ZR(IDPDX1+(I-1)*2*NNO1),
     &       ZR(IDPDY1+(I-1)*2*NNO1), ZR(IDPDZ1+(I-1)*2*NNO1),
     &       ZR(IDSDX1+(I-1)*2*NNO1), ZR(IDSDY1+(I-1)*2*NNO1),
     &       ZR(IDSDZ1+(I-1)*2*NNO1), ZR(IDSXY1+(I-1)*2*NNO1),
     &       ZR(IDSXZ1+(I-1)*2*NNO1), ZR(IDSYZ1+(I-1)*2*NNO1),IDER)
C --- POUR LA GEOMETRIE
        IDER = 2
        CALL CLFFCH ( GEOM, FLUI, NNO1, XG, YG, ZG,
     &       XIN, YIN, ZIN,
     &       ZR(IVF2  +(I-1)*NNO1), ZR(IDPDX2+(I-1)*NNO1),
     &       ZR(IDPDY2+(I-1)*NNO1), ZR(IDPDZ2+(I-1)*NNO1),
     &       ZR(IDSDX2+(I-1)*NNO1), ZR(IDSDY2+(I-1)*NNO1),
     &       ZR(IDSDZ2+(I-1)*NNO1), ZR(IDSXY2+(I-1)*NNO1),
     &       ZR(IDSXZ2+(I-1)*NNO1), ZR(IDSYZ2+(I-1)*NNO1),IDER)
C
 100  CONTINUE
C     --------------------------------------------
C     --- DEUXIEME FAMILLE DE POINTS DE GAUSS ----
C     --- MATRICE DE MASSE : POUTRE           ----
C     --------------------------------------------
      ICOPG  = ICOPG  + 3*NPG
      IPOIDS = IDSYZ2 + NPG * NNO1
      NPG    = NPG1(2,1) * NPG1(2,1) * NPG1(2,2)
      IVF1   = IPOIDS + NPG
      IVF2   = IVF1   + NPG * 2 * NNO1
      IDPDX2 = IVF2   + NPG * NNO1
      IDPDY2 = IDPDX2 + NPG * NNO1
      IDPDZ2 = IDPDY2 + NPG * NNO1
C --- POIDS ET COORDONNEES DES POINTS DE GAUSS
      NPGI(1) = NPG1(2,1)
      NPGI(2) = NPG1(2,1)
      NPGI(3) = NPG1(2,2)
      CALL GAUSCH ( NPGI, ZR(ICOPG), ZR(ICOPG + NPG),
     &              ZR(ICOPG + NPG + NPG), ZR(IPOIDS) )
C --- FONCTIONS DE FORME ET DERIVEES PREMIERES AU POINT DE GAUSS
      DO 200 I = 1, NPG
        XG = ZR(ICOPG+I-1)
        YG = ZR(ICOPG+I-1+NPG)
        ZG = ZR(ICOPG+I-1+NPG+NPG)
C --- POUR LES POUTRES
        IDER = 0
        CALL CLFFCH ( ALIAS, POU, NNO1, XG, YG, ZG,
     &       XIN, YIN, ZIN,
     &       ZR(IVF1+(I-1)*2*NNO1),
     &       BID,BID,BID,BID,BID,BID,BID,BID,BID,IDER)
C --- POUR LA GEOMETRIE
        IDER = 1
        CALL CLFFCH ( GEOM, FLUI, NNO1, XG, YG, ZG,
     &       XIN, YIN, ZIN,
     &       ZR(IVF2  +(I-1)*NNO1),ZR(IDPDX2+(I-1)*NNO1),
     &       ZR(IDPDY2+(I-1)*NNO1),ZR(IDPDZ2+(I-1)*NNO1),
     &       BID,BID,BID,BID,BID,BID,IDER)
C
 200   CONTINUE
C     --------------------------------------------
C     --- TROISIEME FAMILLE DE POINTS DE GAUSS ---
C     --- MATRICE DE MASSE : FLUIDE / FLUIDE   ---
C     --------------------------------------------
      ICOPG  = ICOPG  + 3*NPG
      IPOIDS = IDPDZ2 + NPG * NNO1
      NPG    = NPG2(1,1) * NPG2(1,1) * NPG2(1,2)
      IVF1   = IPOIDS + NPG
      IDPDX1 = IVF1   + NPG * NNO2
      IDPDY1 = IDPDX1 + NPG * NNO2
      IDPDZ1 = IDPDY1 + NPG * NNO2
      IVF2   = IDPDZ1 + NPG * NNO2
      IDPDX2 = IVF2   + NPG * NNO1
      IDPDY2 = IDPDX2 + NPG * NNO1
      IDPDZ2 = IDPDY2 + NPG * NNO1
C --- POIDS ET COORDONNEES DES POINTS DE GAUSS
      NPGI(1) = NPG2(1,1)
      NPGI(2) = NPG2(1,1)
      NPGI(3) = NPG2(1,2)
      CALL GAUSCH ( NPGI, ZR(ICOPG), ZR(ICOPG + NPG),
     &                ZR(ICOPG + NPG + NPG), ZR(IPOIDS) )
C --- FONCTIONS DE FORME ET DERIVEES PREMIERES AU POINT DE GAUSS
      DO 300 I = 1, NPG
          XG = ZR(ICOPG+I-1)
          YG = ZR(ICOPG+I-1+NPG)
          ZG = ZR(ICOPG+I-1+NPG+NPG)
C --- POUR LE FLUIDE
        IDER = 1
        CALL CLFFCH ( ALIAS, FLUI, NNO2, XG, YG, ZG, XIN, YIN, ZIN,
     &       ZR(IVF1  +(I-1)*NNO2),ZR(IDPDX1+(I-1)*NNO2),
     &       ZR(IDPDY1+(I-1)*NNO2),ZR(IDPDZ1+(I-1)*NNO2),
     &       BID,BID,BID,BID,BID,BID,IDER )
C --- POUR LA GEOMETRIE
        IDER = 1
        CALL CLFFCH ( GEOM, FLUI, NNO1, XG, YG, ZG, XIN, YIN, ZIN,
     &       ZR(IVF2  +(I-1)*NNO1),ZR(IDPDX2+(I-1)*NNO1),
     &       ZR(IDPDY2+(I-1)*NNO1),ZR(IDPDZ2+(I-1)*NNO1),
     &       BID,BID,BID,BID,BID,BID,IDER )
C
 300  CONTINUE
C     --------------------------------------------
C     --- QUATRIEME FAMILLE DE POINTS DE GAUSS ---
C     --- MATRICE DE MASSE : SOLIDE / FLUIDE  ----
C     --------------------------------------------
      ICOPG  = ICOPG  + 3*NPG
      IPOIDS = IDPDZ2 + NPG * NNO1
      NPG    = NPG2(2,1) * NPG2(2,1) * NPG2(2,2)
      IVF1   = IPOIDS + NPG
      IVF12  = IVF1   + NPG * 2 * NNO1
      IDPDX1 = IVF12  + NPG * NNO2
      IDPDY1 = IDPDX1 + NPG * NNO2
      IDPDZ1 = IDPDY1 + NPG * NNO2
      IVF2   = IDPDZ1 + NPG * NNO2
      IDPDX2 = IVF2   + NPG * NNO1
      IDPDY2 = IDPDX2 + NPG * NNO1
      IDPDZ2 = IDPDY2 + NPG * NNO1
C --- POIDS ET COORDONNEES DES POINTS DE GAUSS
      NPGI(1) = NPG2(2,1)
      NPGI(2) = NPG2(2,1)
      NPGI(3) = NPG2(2,2)
      CALL GAUSCH ( NPGI, ZR(ICOPG), ZR(ICOPG + NPG),
     &                  ZR(ICOPG + NPG + NPG), ZR(IPOIDS) )
C --- FONCTIONS DE FORME ET DERIVEES PREMIERES AU POINT DE GAUSS
      DO 400 I = 1, NPG
        XG = ZR(ICOPG+I-1)
        YG = ZR(ICOPG+I-1+NPG)
        ZG = ZR(ICOPG+I-1+NPG+NPG)
C --- POUR LES POUTRES
        IDER = 0
        CALL CLFFCH( ALIAS, POU, NNO1, XG, YG, ZG,
     &       XIN, YIN, ZIN,
     &       ZR(IVF1+(I-1)*2*NNO1),
     &       BID,BID,BID,BID,BID,BID,BID,BID,BID,IDER )
C --- POUR LE FLUIDE
        IDER = 1
        CALL CLFFCH( ALIAS, FLUI, NNO2, XG, YG, ZG, XIN, YIN, ZIN,
     &       ZR(IVF12 +(I-1)*NNO2),ZR(IDPDX1+(I-1)*NNO2),
     &       ZR(IDPDY1+(I-1)*NNO2),ZR(IDPDZ1+(I-1)*NNO2),
     &       BID,BID,BID,BID,BID,BID,IDER )
C --- POUR LA GEOMETRIE
        IDER = 1
        CALL CLFFCH( GEOM, FLUI, NNO1, XG, YG, ZG, XIN, YIN, ZIN,
     &       ZR(IVF2  +(I-1)*NNO1),ZR(IDPDX2+(I-1)*NNO1),
     &       ZR(IDPDY2+(I-1)*NNO1),ZR(IDPDZ2+(I-1)*NNO1),
     &       BID,BID,BID,BID,BID,BID,IDER )
C
 400   CONTINUE
C
      CALL JEDETR ('&&JNI014.FFT')
C
9998  CONTINUE

C
      END
