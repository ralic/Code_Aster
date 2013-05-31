subroutine xmolig(liel1, trav)
! aslint: disable=W1501
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/xmoajo.h'
    include 'asterfort/xmoimp.h'
    include 'asterfort/xmoini.h'
    character(len=24) :: liel1, trav
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM APPELEE PAR MODI_MODELE_XFEM (OP0113)
!
!    MODIFICATION DU LIGREL X-FEM SUIVANT LE TYPE D'ENRICHISSMENT
!
! ----------------------------------------------------------------------
!
!
! IN      LIEL1  : LIGREL DU MODELE SAIN
! IN/OUT  TRAV   : TABLEAU DE TRAVAIL  CONTENANT LES TYPES
!                  D'ENRICHISSEMENT ET LE TYPE DES NOUVEAUX ELEMENTS
!
!
!
!
!
    integer :: ih8(13), ip6(13), ip5(13), it4(13)
    integer :: ih20(6), ip15(6), ip13(6), it10(6)
    integer :: icpq4(13), icpt3(13), idpq4(13), idpt3(13)
    integer :: icpq8(6), icpt6(6), idpq8(6), idpt6(6)
    integer :: if4(10), if3(10), ipf2(10)
    integer :: if8(3), if6(3), ipf3(3)
!
    integer :: nh8(14), nh20(7), np6(14), np15(7), np5(14), np13(7)
    integer :: nt4(14), nt10(7)
    integer :: ncpq4(14), ncpq8(7), ncpt3(14), ncpt6(7), ndpq4(14)
    integer :: ndpq8(7), ndpt3(14), ndpt6(7), nf4(11), nf8(7), nf3(11)
    integer :: nf6(7), npf2(11), npf3(7)
!
    integer :: iaxt3(6), iaxq4(6), iaxq8(6), iaxt6(6), iax2(3), iax3(3)
    integer :: naxt3(7), naxq4(7), naxq8(7), naxt6(7), nax2(7), nax3(7)
!
    integer :: ith8(3), itp6(3), itp5(3), itt4(3), itpq4(3), itpt3(3)
    integer :: nth8(7), ntp6(7), ntp5(7), ntt4(7), ntpq4(7), ntpt3(7)
    integer :: itaq4(3), itat3(3), itf4(3), itf3(3), itpf2(3), itax2(3)
    integer :: ntaq4(7), ntat3(7), ntf4(7), ntf3(7), ntpf2(7), ntax2(7)
!
    integer :: jtab, ngr1, igr1, j1, n1, nbelt, itypel, iel, ima, jj
    integer :: jnbsp, nfiss, i
    character(len=8) :: k8bid
    character(len=16) :: notype
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     INITIALISATIONS DE TOUS LES COMPTEURS
    call xmoini(nh8, nh20, np6, np15, np5,&
                np13, nt4, nt10, ncpq4, ncpq8,&
                ncpt3, ncpt6, ndpq4, ndpq8, ndpt3,&
                ndpt6, nf4, nf8, nf3, nf6,&
                npf2, npf3, naxt3, naxq4, naxq8,&
                naxt6, nax2, nax3, nth8, ntp6,&
                ntp5, ntt4, ntpq4, ntpt3, ntaq4,&
                ntat3, ntf4, ntf3, ntpf2, ntax2)
!
! ----------------------------------------------------------------------
!     ELEMENTS MECANIQUES
! ----------------------------------------------------------------------
!
!     ELEMENT PRINCIPAUX 3D LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_HEXA8' ), ih8(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_HEXA8' ), ih8(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_HEXA8'), ih8(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_HEXA8' ), ih8(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_HEXA8' ), ih8(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_HEXA8'), ih8(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH1_HEXA8' ), ih8(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2_HEXA8' ), ih8(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3_HEXA8' ), ih8(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4_HEXA8' ), ih8(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2C_HEXA8' ), ih8(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3C_HEXA8' ), ih8(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4C_HEXA8' ), ih8(13))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_PENTA6' ), ip6(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_PENTA6' ), ip6(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_PENTA6'), ip6(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_PENTA6' ), ip6(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_PENTA6' ), ip6(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_PENTA6'), ip6(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH1_PENTA6' ), ip6(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2_PENTA6' ), ip6(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3_PENTA6' ), ip6(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4_PENTA6' ), ip6(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2C_PENTA6' ), ip6(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3C_PENTA6' ), ip6(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4C_PENTA6' ), ip6(13))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_PYRAM5' ), ip5(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_PYRAM5' ), ip5(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_PYRAM5'), ip5(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_PYRAM5' ), ip5(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_PYRAM5' ), ip5(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_PYRAM5'), ip5(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH1_PYRAM5' ), ip5(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2_PYRAM5' ), ip5(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3_PYRAM5' ), ip5(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4_PYRAM5' ), ip5(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2C_PYRAM5' ), ip5(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3C_PYRAM5' ), ip5(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4C_PYRAM5' ), ip5(13))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_TETRA4' ), it4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_TETRA4' ), it4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_TETRA4'), it4(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_TETRA4' ), it4(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_TETRA4' ), it4(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_TETRA4'), it4(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH1_TETRA4' ), it4(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2_TETRA4' ), it4(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3_TETRA4' ), it4(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4_TETRA4' ), it4(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2C_TETRA4' ), it4(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3C_TETRA4' ), it4(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4C_TETRA4' ), it4(13))
!
!     ELEMENT PRINCIPAUX 3D QUADRATIQUES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_HEXA20' ), ih20(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_HEXA20' ), ih20(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_HEXA20'), ih20(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_HEXA20' ), ih20(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_HEXA20' ), ih20(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_HE20'), ih20(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_PENTA15' ), ip15(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_PENTA15' ), ip15(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_PENTA15'), ip15(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_PENTA15' ), ip15(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_PENTA15' ), ip15(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_PE15'), ip15(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_PYRAM13' ), ip13(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_PYRAM13' ), ip13(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_PYRAM13'), ip13(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_PYRAM13' ), ip13(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_PYRAM13' ), ip13(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_PY13'), ip13(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_TETRA10' ), it10(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_TETRA10' ), it10(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_TETRA10'), it10(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHC_TETRA10' ), it10(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XTC_TETRA10' ), it10(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHTC_TE10'), it10(6))
!
!     ELEMENT PRINCIPAUX 2D (CP/DP) LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH' ), icpq4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XT' ), icpq4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XHT' ), icpq4(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XHC' ), icpq4(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XTC' ), icpq4(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XHTC'), icpq4(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH1' ), icpq4(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH2' ), icpq4(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH3' ), icpq4(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH4' ), icpq4(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH2C'), icpq4(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH3C'), icpq4(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4_XH4C'), icpq4(13))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH' ), icpt3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XT' ), icpt3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XHT' ), icpt3(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XHC' ), icpt3(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XTC' ), icpt3(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XHTC'), icpt3(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH1' ), icpt3(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH2' ), icpt3(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH3' ), icpt3(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH4' ), icpt3(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH2C'), icpt3(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH3C'), icpt3(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3_XH4C'), icpt3(13))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH' ), idpq4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XT' ), idpq4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XHT' ), idpq4(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XHC' ), idpq4(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XTC' ), idpq4(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XHTC'), idpq4(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH1' ), idpq4(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH2' ), idpq4(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH3' ), idpq4(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH4' ), idpq4(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH2C'), idpq4(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH3C'), idpq4(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4_XH4C'), idpq4(13))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH' ), idpt3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XT' ), idpt3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XHT' ), idpt3(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XHC' ), idpt3(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XTC' ), idpt3(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XHTC'), idpt3(6))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH1' ), idpt3(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH2' ), idpt3(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH3' ), idpt3(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH4' ), idpt3(10))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH2C'), idpt3(11))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH3C'), idpt3(12))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3_XH4C'), idpt3(13))
!
!     ELEMENT PRINCIPAUX AXIS LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3_XH' ), iaxt3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3_XT' ), iaxt3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3_XHT'), iaxt3(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3_XHC' ), iaxt3(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3_XTC' ), iaxt3(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3_XHTC'), iaxt3(6))
!      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MEAXTR3_XH1' ),IAXT3(7))
!      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MEAXTR3_XH2' ),IAXT3(8))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4_XH' ), iaxq4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4_XT' ), iaxq4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4_XHT'), iaxq4(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4_XHC' ), iaxq4(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4_XTC' ), iaxq4(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4_XHTC'), iaxq4(6))
!      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MEAXQU4_XH1'),IAXQ4(7))
!      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MEAXQU4_XH2'),IAXQ4(8))
!
!     ELEMENT PRINCIPAUX 2D (CP/DP) QUADRATIQUES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU8_XH' ), icpq8(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU8_XT' ), icpq8(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU8_XHT'), icpq8(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU8_XHC' ), icpq8(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU8_XTC' ), icpq8(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU8_XHTC'), icpq8(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR6_XH' ), icpt6(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR6_XT' ), icpt6(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR6_XHT'), icpt6(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR6_XHC' ), icpt6(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR6_XTC' ), icpt6(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR6_XHTC'), icpt6(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU8_XH' ), idpq8(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU8_XT' ), idpq8(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU8_XHT'), idpq8(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU8_XHC' ), idpq8(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU8_XTC' ), idpq8(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU8_XHTC'), idpq8(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR6_XH' ), idpt6(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR6_XT' ), idpt6(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR6_XHT'), idpt6(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR6_XHC' ), idpt6(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR6_XTC' ), idpt6(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR6_XHTC'), idpt6(6))
!
!     ELEMENT PRINCIPAUX AXIS QUADRATIQUES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU8_XH' ), iaxq8(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU8_XT' ), iaxq8(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU8_XHT'), iaxq8(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU8_XHC' ), iaxq8(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU8_XTC' ), iaxq8(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU8_XHTC'), iaxq8(6))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR6_XH' ), iaxt6(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR6_XT' ), iaxt6(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR6_XHT'), iaxt6(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR6_XHC' ), iaxt6(4))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR6_XTC' ), iaxt6(5))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR6_XHTC'), iaxt6(6))
!
!     ELEMENT DE BORD 3D LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_FACE4' ), if4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_FACE4' ), if4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_FACE4'), if4(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH1_FACE4'), if4(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2_FACE4'), if4(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3_FACE4'), if4(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4_FACE4'), if4(10))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_FACE3' ), if3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_FACE3' ), if3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_FACE3'), if3(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH1_FACE3' ), if3(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH2_FACE3' ), if3(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH3_FACE3' ), if3(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH4_FACE3' ), if3(10))
!
!     ELEMENT DE BORD 3D QUADRATIQUES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_FACE8' ), if8(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_FACE8' ), if8(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_FACE8'), if8(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XH_FACE6' ), if6(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XT_FACE6' ), if6(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_XHT_FACE6'), if6(3))
!
!     ELEMENT DE BORD 2D (QUE DP) LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XH' ), ipf2(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XT' ), ipf2(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XHT'), ipf2(3))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XH1' ), ipf2(7))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XH2' ), ipf2(8))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XH3' ), ipf2(9))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE2_XH4' ), ipf2(10))
!
!     ELEMENT DE BORD 2D (QUE DP) QUADRATIQUES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE3_XH' ), ipf3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE3_XT' ), ipf3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEPLSE3_XHT'), ipf3(3))
!
!     ELEMENT DE BORD AXIS LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXSE2_XH' ), iax2(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXSE2_XT' ), iax2(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXSE2_XHT'), iax2(3))
!
!     ELEMENT DE BORD AXIS QUADRATIQUES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXSE3_XH' ), iax3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXSE3_XT' ), iax3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXSE3_XHT'), iax3(3))
!
! ----------------------------------------------------------------------
!     ELEMENTS THERMIQUES
! ----------------------------------------------------------------------
!
!     ELEMENT PRINCIPAUX 3D LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XH_HEXA8' ), ith8(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XT_HEXA8' ), ith8(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XHT_HEXA8'), ith8(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XH_PENTA6' ), itp6(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XT_PENTA6' ), itp6(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XHT_PENTA6'), itp6(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XH_PYRAM5' ), itp5(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XT_PYRAM5' ), itp5(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XHT_PYRAM5'), itp5(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XH_TETRA4' ), itt4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XT_TETRA4' ), itt4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XHT_TETRA4'), itt4(3))
!
!     ELEMENT PRINCIPAUX 2D (PLAN) LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLQU4_XH' ), itpq4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLQU4_XT' ), itpq4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLQU4_XHT' ), itpq4(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLTR3_XH' ), itpt3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLTR3_XT' ), itpt3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLTR3_XHT' ), itpt3(3))
!
!     ELEMENT PRINCIPAUX AXI LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXQU4_XH' ), itaq4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXQU4_XT' ), itaq4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXQU4_XHT' ), itaq4(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXTR3_XH' ), itat3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXTR3_XT' ), itat3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXTR3_XHT' ), itat3(3))
!
!     ELEMENT DE BORD 3D LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XH_FACE4' ), itf4(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XT_FACE4' ), itf4(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XHT_FACE4'), itf4(3))
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XH_FACE3' ), itf3(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XT_FACE3' ), itf3(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THER_XHT_FACE3'), itf3(3))
!
!     ELEMENT DE BORD 2D (PLAN) LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLSE2_XH' ), itpf2(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLSE2_XT' ), itpf2(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THPLSE2_XHT'), itpf2(3))
!
!     ELEMENT DE BORD AXIS LINEAIRES
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXSE2_XH' ), itax2(1))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXSE2_XT' ), itax2(2))
    call jenonu(jexnom('&CATA.TE.NOMTE', 'THAXSE2_XHT'), itax2(3))
!
!     RECUPERATION DE L'ADRESSE DU TABLEAU DE TRAVAIL
    call jeveuo(trav, 'E', jtab)
!
! --- RECUPERATION DU NOMBRE DE SOUS POINT (NBRE DE FISSURES VUES)
!
    call jeveuo('&&XTYELE.NBSP', 'L', jnbsp)
!
!     REMPLISSAGE DE LA 5EME COLONNE DU TABLEAU AVEC LE TYPE D'ELEMENT
    call jelira(liel1, 'NMAXOC', ngr1, k8bid)
!
    do 200 igr1 = 1, ngr1
        call jeveuo(jexnum(liel1, igr1), 'L', j1)
        call jelira(jexnum(liel1, igr1), 'LONMAX', n1, k8bid)
        nbelt=n1-1
        itypel=zi(j1-1+n1)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), notype)
        do 210 iel = 1, nbelt
            ima=zi(j1-1+iel)
            jj=jtab-1+5*(ima-1)
            nfiss = zi(jnbsp-1+ima)
            if (zi(jj+4) .eq. 0) then
! --- ELEMENTS X-FEM MECANIQUES
                if (notype .eq. 'MECA_HEXA8') then
                    call xmoajo(jj, nfiss, ih8, nh8)
                else if (notype.eq.'MECA_HEXA20') then
                    call xmoajo(jj, nfiss, ih20, nh20)
                else if (notype.eq.'MECA_PENTA6') then
                    call xmoajo(jj, nfiss, ip6, np6)
                else if (notype.eq.'MECA_PENTA15') then
                    call xmoajo(jj, nfiss, ip15, np15)
                else if (notype.eq.'MECA_PYRAM5') then
                    call xmoajo(jj, nfiss, ip5, np5)
                else if (notype.eq.'MECA_PYRAM13') then
                    call xmoajo(jj, nfiss, ip13, np13)
                else if (notype.eq.'MECA_TETRA4') then
                    call xmoajo(jj, nfiss, it4, nt4)
                else if (notype.eq.'MECA_TETRA10') then
                    call xmoajo(jj, nfiss, it10, nt10)
                else if (notype.eq.'MECPQU4') then
                    call xmoajo(jj, nfiss, icpq4, ncpq4)
                else if (notype.eq.'MECPQU8') then
                    call xmoajo(jj, nfiss, icpq8, ncpq8)
                else if (notype.eq.'MECPTR3') then
                    call xmoajo(jj, nfiss, icpt3, ncpt3)
                else if (notype.eq.'MECPTR6') then
                    call xmoajo(jj, nfiss, icpt6, ncpt6)
                else if (notype.eq.'MEDPQU4') then
                    call xmoajo(jj, nfiss, idpq4, ndpq4)
                else if (notype.eq.'MEDPQU8') then
                    call xmoajo(jj, nfiss, idpq8, ndpq8)
                else if (notype.eq.'MEDPTR3') then
                    call xmoajo(jj, nfiss, idpt3, ndpt3)
                else if (notype.eq.'MEDPTR6') then
                    call xmoajo(jj, nfiss, idpt6, ndpt6)
                else if (notype.eq.'MECA_FACE4') then
                    call xmoajo(jj, nfiss, if4, nf4)
                else if (notype.eq.'MECA_FACE8') then
                    call xmoajo(jj, nfiss, if8, nf8)
                else if (notype.eq.'MECA_FACE3') then
                    call xmoajo(jj, nfiss, if3, nf3)
                else if (notype.eq.'MECA_FACE6') then
                    call xmoajo(jj, nfiss, if6, nf6)
                else if (notype.eq.'MEPLSE2') then
                    call xmoajo(jj, nfiss, ipf2, npf2)
                else if (notype.eq.'MEPLSE3') then
                    call xmoajo(jj, nfiss, ipf3, npf3)
                else if (notype.eq.'MEAXQU4') then
                    call xmoajo(jj, nfiss, iaxq4, naxq4)
                else if (notype.eq.'MEAXQU8') then
                    call xmoajo(jj, nfiss, iaxq8, naxq8)
                else if (notype.eq.'MEAXTR3') then
                    call xmoajo(jj, nfiss, iaxt3, naxt3)
                else if (notype.eq.'MEAXTR6') then
                    call xmoajo(jj, nfiss, iaxt6, naxt6)
                else if (notype.eq.'MEAXSE2') then
                    call xmoajo(jj, nfiss, iax2, nax2)
                else if (notype.eq.'MEAXSE3') then
                    call xmoajo(jj, nfiss, iax3, nax3)
! --- ELEMENTS X-FEM THERMIQUES
                else if (notype.eq.'THER_HEXA8') then
                    call xmoajo(jj, nfiss, ith8, nth8)
                else if (notype.eq.'THER_PENTA6') then
                    call xmoajo(jj, nfiss, itp6, ntp6)
                else if (notype.eq.'THER_PYRAM5') then
                    call xmoajo(jj, nfiss, itp5, ntp5)
                else if (notype.eq.'THER_TETRA4') then
                    call xmoajo(jj, nfiss, itt4, ntt4)
                else if (notype.eq.'THPLQU4') then
                    call xmoajo(jj, nfiss, itpq4, ntpq4)
                else if (notype.eq.'THPLTR3') then
                    call xmoajo(jj, nfiss, itpt3, ntpt3)
                else if (notype.eq.'THAXQU4') then
                    call xmoajo(jj, nfiss, itaq4, ntaq4)
                else if (notype.eq.'THAXTR3') then
                    call xmoajo(jj, nfiss, itat3, ntat3)
                else if (notype.eq.'THER_FACE4') then
                    call xmoajo(jj, nfiss, itf4, ntf4)
                else if (notype.eq.'THER_FACE3') then
                    call xmoajo(jj, nfiss, itf3, ntf3)
                else if (notype.eq.'THPLSE2') then
                    call xmoajo(jj, nfiss, itpf2, ntpf2)
                else if (notype.eq.'THAXSE2') then
                    call xmoajo(jj, nfiss, itax2, ntax2)
!
!         ELEMENTS QUI RESTE X-FEM SI LE MODELE DE DEPART EST X-FEM
!         -----------------------------------------------
                else
                    zi(jj+5)=itypel
                endif
            else
!         ELEMENTS NON X-FEM
!         ------------------
                if (notype(5:6) .eq. '_X') then
!         ELEMENT QUI DEVIENT CLASSIQUE SI LE MODELE DE DEPART EST X-FEM
!         NOTONS QUE CETTE ETAPE N'EST UTILE QUE POUR PROPA_FISS, QUI
!         REPART DU MODELE XFEM PRECEDENT PLUTOT QUE DU MODELE INITIAL.
!         ON POURRAIT REPARTIR DU MODELE INITIAL DANS PROPA_FISS ET NE
!         PAS AVOIR A FAIRE CETTE ÉTAPE.
                    do 220 i = 1, 6
                        if (itypel .eq. ih8(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_HEXA8'), itypel)
                        else if (itypel.eq.ip6(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_PENTA6'), itypel)
                        else if (itypel.eq.ip5(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_PYRAM5'), itypel)
                        else if (itypel.eq.it4(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MECA_TETRA4'), itypel)
                        endif
220                  continue
                else if (notype(8:9).eq.'_X') then
                    do 230 i = 1, 6
                        if (itypel .eq. icpq4(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPQU4'), itypel)
                        else if (itypel.eq.icpt3(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MECPTR3'), itypel)
                        else if (itypel.eq.idpq4(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPQU4'), itypel)
                        else if (itypel.eq.idpt3(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDPTR3'), itypel)
                        else if (itypel.eq.iaxq4(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXQU4'), itypel)
                        else if (itypel.eq.iaxt3(i)) then
                            call jenonu(jexnom('&CATA.TE.NOMTE', 'MEAXTR3'), itypel)
                        endif
230                  continue
                endif
                zi(jj+5)=itypel
            endif
!             EN MECANIQUE, ERREUR SI UN SEG DOIT ETRE ENRICHI EN 3D
!             CAR LES ELEMENTS X-FEM DE BORD SEG N'EXISTENT PAS EN 3D
!             RQ: POUR LA THERMIQUE, LE PROBLEME NE SE POSE PAS CAR
!             --  EN FEM 3D, IL N'EXISTE PAS D'ELEMENTS DE DIMENSION 1
!
            if (notype(1:10) .eq. 'MECA_ARETE' .and. zi(jj+4) .eq. 0) call u2mess('F', 'XFEM_13')
!
210      continue
200  end do
!
!     IMPRESSIONS
    call xmoimp(nh8, nh20, np6, np15, np5,&
                np13, nt4, nt10, ncpq4, ncpq8,&
                ncpt3, ncpt6, ndpq4, ndpq8, ndpt3,&
                ndpt6, nf4, nf8, nf3, nf6,&
                npf2, npf3, naxt3, naxq4, naxq8,&
                naxt6, nax2, nax3, nth8, ntp6,&
                ntp5, ntt4, ntpq4, ntpt3, ntaq4,&
                ntat3, ntf4, ntf3, ntpf2, ntax2)
!
    call jedema()
end subroutine
