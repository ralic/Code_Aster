subroutine pjelco(moa1, moa2, cham1, corres, base)
    implicit   none
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     COMMANDE:  PROJ_CHAMP /  METHODE='ECLA_PG'
! BUT : CALCULER LA STRUCTURE DE DONNEE CORRESP_2_MAILLA
!       DANS LE CAS OU IL Y A UN CHAM_ELGA A TRAITER (CHAM1)
! ----------------------------------------------------------------------
    include 'asterfort/assert.h'
    include 'asterfort/cargeo.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/pjefco.h'
    include 'asterfort/pjma1p.h'
    include 'asterfort/pjma2p.h'
    include 'asterfort/u2mesi.h'
    character(len=8) :: moa1, moa2
    character(len=16) :: corres
    character(len=19) :: cham1
    character(len=1) :: base
    character(len=8) :: ma1p, ma2p, kbid
    integer :: ndim, ndim1, ndim2, ibid
!     ----------------------------------------------
!
    call assert(base.eq.'V')
!
!
!     -- CALCUL DE NDIM :
    call dismoi('F', 'DIM_GEOM', moa1, 'MODELE', ndim1,&
                kbid, ibid)
    call dismoi('F', 'DIM_GEOM', moa2, 'MODELE', ndim2,&
                kbid, ibid)
    call assert(ndim1.eq.ndim2)
    ndim=ndim1
    call assert(ndim.eq.2.or.ndim.eq.3)
    call u2mesi('I', 'CALCULEL3_28', 1, ndim)
!
!
!     CREATION DU MAILLAGE 1 PRIME (MA1P)
!     REMPLISSAGE DU .PJEF_MP DANS LA SD CORRES
!     QUI EST LE NOM DU MAILLAGE 1 PRIME
!     ----------------------------------------------
    ma1p='&&PJELC1'
    call pjma1p(moa1, ma1p, cham1, corres)
    call cargeo(ma1p)
!
!
!     CREATION DU MAILLAGE 2 PRIME (MA2P)
!     REMPLISSAGE DU .PJEF_EL DANS LA SD CORRES
!     QUI EST UN TABLEAU REFERENCANT, POUR CHAQUE ELGA,
!     SON NUMERO ET LE NUMERO DE LA MAILLE A LAQUELLE IL APPARTIENT
!     ----------------------------------------------
    ma2p='&&PJELC2'
    call pjma2p(ndim, moa2, ma2p, corres)
!
!     -- APPEL A LA ROUTINE "USUELLE" PJEFCO
!        AVEC LES DEUX MAILLAGES PRIME
!     ----------------------------------------------
    call pjefco(ma1p, ma2p, corres, 'V')
!
end subroutine
