      SUBROUTINE MECAL2(OPTION,MODELE,CHDEPL,CHGEOM,CHMATE,CHCARA,
     &                  CHTEMP,CHTREF,CHTIME,CHNUMC,CHHARM,CHFREQ,
     &                  CHMASS,THETA1,ALPHA,CHARGE,CHDYNR,SUROPT,CHELEM,
     &                  LIGREL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) OPTION,MODELE,CHDEPL,CHDYNR,SUROPT,CHELEM
      CHARACTER*(*) CHGEOM,CHMATE,CHCARA(*),CHFREQ,CHMASS,LIGREL
      CHARACTER*(*) CHTEMP,CHTREF,CHTIME,CHNUMC,CHHARM,CHARGE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     - FONCTION REALISEE : APPEL A "CALCUL"
C                           CALCUL DES CONTRAINTES ELEMENTAIRES
C                           AVEC LA METHODE LAGRANGIENNE.
C ----------------------------------------------------------------------
C IN  : OPTION : OPTION DE CALCUL
C IN  : MODELE : NOM DU MODELE
C IN  : CH...  : NOM DES CHAMPS ...
C IN  : CHARGE : NOM D'UNE CHARGE
C OUT : CHELEM : CHAMELEM RESULTAT
C ----------------------------------------------------------------------
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='MECAL2')

      CHARACTER*1 TYPCOE
      CHARACTER*8 K8B,NOMODE,LPAIN(30),LPAOUT(1),NOMA,CHA
      CHARACTER*8 KBID
      CHARACTER*16 OPTIO2
      CHARACTER*24 LCHIN(30),LCHOUT(1),CHDEP2,CHELE2
      CHARACTER*24 LIGRCH
      CHARACTER*24 THETA1,CHALPH
      REAL*8 ALPHA
      COMPLEX*16 CCOEF
      LOGICAL LFONC
C DEB-------------------------------------------------------------------

      CHDEP2 = CHDEPL
      CHELE2 = CHELEM
      NOMODE = MODELE
      OPTIO2 = OPTION
      NOMA = CHGEOM(1:8)
      COEF = 1.D0
      CCOEF = (1.D0,1.D0)
      TYPCOE = ' '

      IF (MODELE(1:1).EQ.' ') CALL UTMESS('F',NOMPRO,
     &                             'IL FAUT UN MODELE.')

      LCHIN(1) = CHDEP2
      LCHOUT(1) = CHELE2

      IF (OPTIO2.EQ.'SIEF_ELNO_ELGA') THEN
        LPAIN(1) = 'PCONTRR'
        LPAOUT(1) = 'PSIEFNOR'
        NBIN = 1

      ELSE IF (OPTIO2.EQ.'SIGM_ELNO_CART') THEN
        LPAIN(1) = 'PCONTRR'
        LPAOUT(1) = 'PCONTGL'
        LPAIN(2) = 'PGEOMER'
        LCHIN(2) = CHGEOM
        LPAIN(3) = 'PCAORIE'
        LCHIN(3) = CHCARA(1)
        LPAIN(4) = 'PCAARPO'
        LCHIN(4) = CHCARA(9)
        NBIN = 4

      ELSE IF (OPTIO2.EQ.'VNOR_ELEM_DEPL') THEN
        LPAIN(1) = 'PDEPLAC'
        LPAOUT(1) = 'PVITNOR'
        LPAIN(2) = 'PGEOMER'
        LCHIN(2) = CHGEOM
        NBIN = 2

      ELSE IF (OPTIO2.EQ.'EFGE_ELNO_CART') THEN
        LPAIN(1) = 'PCONTRR'
        LPAOUT(1) = 'PEFFORR'
        LPAIN(2) = 'PGEOMER'
        LCHIN(2) = CHGEOM
        LPAIN(3) = 'PCAORIE'
        LCHIN(3) = CHCARA(1)
        LPAIN(4) = 'PCAARPO'
        LCHIN(4) = CHCARA(9)
        NBIN = 4

      ELSE
        LPAIN(1) = 'PDEPLAR'
        LPAIN(2) = 'PGEOMER'
        LCHIN(2) = CHGEOM
        LPAIN(3) = 'PMATERC'
        LCHIN(3) = CHMATE
        LPAIN(4) = 'PTEMPER'
        LCHIN(4) = CHTEMP
        LPAIN(5) = 'PTEREF'
        LCHIN(5) = CHTREF
        LPAIN(6) = 'PTEMPSR'
        LCHIN(6) = CHTIME
        LPAIN(7) = 'PNUMCOR'
        LCHIN(7) = CHNUMC
        LPAIN(8) = 'PHARMON'
        LCHIN(8) = CHHARM
        LPAIN(9) = 'PCAORIE'
        LCHIN(9) = CHCARA(1)
        LPAIN(10) = 'PCADISK'
        LCHIN(10) = CHCARA(2)
        LPAIN(11) = 'PCAGNPO'
        LCHIN(11) = CHCARA(6)
        LPAIN(12) = 'PCACOQU'
        LCHIN(12) = CHCARA(7)

        IF (OPTIO2.EQ.'DEGE_ELNO_DEPL') THEN
          LPAIN(4) = 'PCACOQU'
          LCHIN(4) = CHCARA(7)
          LPAOUT(1) = 'PDEFOGR'
          NBIN = 4

        ELSE IF (OPTIO2.EQ.'SIGM_ELNO_DEPL' .OR.
     &           OPTIO2.EQ.'SIEF_ELGA_DEPL') THEN
          LPAIN(13) = 'PCAARPO'
          LCHIN(13) = CHCARA(9)
          LPAOUT(1) = 'PCONTRR'
          NBIN = 13

C           RAJOUT DES POUTRES POUX
          CALL DISMOI('F','EXI_RDM',NOMODE,'MODELE',IBID,K8B,IER)
          IF (K8B.EQ.'OUI') THEN
            CALL MECHPO(OPTIO2,'&&'//NOMPRO,CHARGE,MODELE,CHDEP2,CHDYNR,
     &                  SUROPT,LPAIN(14),LCHIN(14),NB,TYPCOE,COEF,CCOEF)
            NBIN = NBIN + NB
          END IF

        ELSE IF (OPTIO2.EQ.'SIGM_ELNO_LAGR' .OR.
     &           OPTIO2.EQ.'SIEF_ELGA_LAGR') THEN
          CALL MEALPH(NOMA,ALPHA,CHALPH)
          LPAIN(13) = 'PCAARPO'
          LCHIN(13) = CHCARA(9)
          LPAOUT(1) = 'PCONTRR'
          LPAIN(14) = 'PTHETAR'
          LCHIN(14) = THETA1
          LPAIN(15) = 'PALPHAR'
          LCHIN(15) = CHALPH
          NBIN = 15

          CALL GETVID(' ','CHARGE',0,1,0,CHA,NCHA)
          IRET5 = 0
          IF (NCHA.NE.0) THEN

            CALL DISMOI('F','TYPE_CHARGE',CHARGE,'CHARGE',IBID,KBID,
     &                  IERD)
            IF (KBID(5:7).EQ.'_FO') THEN
              LFONC = .TRUE.
            ELSE
              LFONC = .FALSE.
            END IF
            LIGRCH = CHARGE//'.CHME.LIGRE'
            CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.EPSIN',IRET5)
          END IF


          IF (IRET5.NE.0) THEN
            IF (LFONC) THEN
              IF (OPTIO2(6:9).EQ.'ELNO') THEN
                OPTIO2 = 'SIGM_ELNO_LGDE_F'
                LPAIN(16) = 'PEPSINF'
              ELSE IF (OPTIO2(6:9).EQ.'ELGA') THEN
                OPTIO2 = 'SIEF_ELGA_LGDE_F'
                LPAIN(16) = 'PEPSINF'
              END IF
            ELSE
              IF (OPTIO2(6:9).EQ.'ELNO') THEN
                OPTIO2 = 'SIGM_ELNO_LGDE_R'
                LPAIN(16) = 'PEPSINR'
              ELSE IF (OPTIO2(6:9).EQ.'ELGA') THEN
                OPTIO2 = 'SIEF_ELGA_LGDE_R'
                LPAIN(16) = 'PEPSINR'
              END IF
            END IF
            LCHIN(16) = LIGRCH(1:13)//'.EPSIN.DESC'
            NBIN = 16

C           RAJOUT DES POUTRES POUX
            CALL DISMOI('F','EXI_RDM',NOMODE,'MODELE',IBID,K8B,IER)
            IF (K8B.EQ.'OUI') THEN
              CALL MECHPO(OPTIO2,'&&'//NOMPRO,CHARGE,MODELE,CHDEP2,
     &                    CHDYNR,SUROPT,LPAIN(17),LCHIN(17),NB,TYPCOE,
     &                    COEF,CCOEF)
              NBIN = NBIN + NB
            END IF
          ELSE

C           RAJOUT DES POUTRES POUX
            CALL DISMOI('F','EXI_RDM',NOMODE,'MODELE',IBID,K8B,IER)
            IF (K8B.EQ.'OUI') THEN
              CALL MECHPO(OPTIO2,'&&'//NOMPRO,CHARGE,MODELE,CHDEP2,
     &                    CHDYNR,SUROPT,LPAIN(16),LCHIN(16),NB,TYPCOE,
     &                    COEF,CCOEF)
              NBIN = NBIN + NB
            END IF
          END IF

        ELSE IF (OPTIO2.EQ.'EFGE_ELNO_DEPL') THEN
          LPAIN(13) = 'PCASECT'
          LCHIN(13) = CHCARA(8)
          LPAIN(14) = 'PCAARPO'
          LCHIN(14) = CHCARA(9)
          LPAOUT(1) = 'PEFFORR'
          NBIN = 14

C           RAJOUT DES POUTRES POUX
          CALL DISMOI('F','EXI_RDM',NOMODE,'MODELE',IBID,K8B,IER)
          IF (K8B.EQ.'OUI') THEN
            CALL MECHPO(OPTIO2,'&&'//NOMPRO,CHARGE,MODELE,CHDEP2,CHDYNR,
     &                  SUROPT,LPAIN(15),LCHIN(15),NB,TYPCOE,COEF,CCOEF)
            NBIN = NBIN + NB
          END IF

        ELSE IF (OPTIO2.EQ.'EPSI_ELNO_DEPL') THEN
          LPAOUT(1) = 'PDEFORR'
          NBIN = 12

        ELSE IF (OPTIO2.EQ.'EPOT_ELEM_DEPL' .OR.
     &           OPTIO2.EQ.'EPOT_ELEM_TEMP') THEN
          LPAIN(13) = 'PCAARPO'
          LCHIN(13) = CHCARA(9)
          LPAOUT(1) = 'PENERDR'
          NBIN = 13

        ELSE IF (OPTIO2.EQ.'ECIN_ELEM_DEPL') THEN
          LPAIN(10) = 'PCADISM'
          LCHIN(10) = CHCARA(3)
          LPAIN(13) = 'PCAARPO'
          LCHIN(13) = CHCARA(9)
          LPAIN(14) = 'PMASDIA'
          LCHIN(14) = CHMASS
          LPAIN(15) = 'PFREQR'
          LCHIN(15) = CHFREQ
          LPAOUT(1) = 'PENERCR'
          NBIN = 15

        ELSE IF (OPTIO2.EQ.'FLUX_ELNO_TEMP' .OR.
     &           OPTIO2.EQ.'FLUX_ELGA_TEMP') THEN
          LPAOUT(1) = 'PFLUX_R'
          NBIN = 12

        ELSE
          CALL UTDEBM('F',NOMPRO,' ')
          CALL UTIMPK('S','OPTION INCONNUE',1,OPTIO2)
          CALL UTFINM()
        END IF
      END IF

      CALL CALCUL('S',OPTIO2,LIGREL,NBIN,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &            'G')
      CALL JEDETC(' ','&&'//NOMPRO,1)

      END
