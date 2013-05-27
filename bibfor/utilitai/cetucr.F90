subroutine cetucr(motfac, model0)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! GRANDEURS CARACTERISTIQUES DE L'ETUDE - CREATION
!           *                     ***     **
!     ------------------------------------------------------------------
!      CREATION D'UNE TABLE CONTENANT LES GRANDEURS CARACTERISTIQUES
!      CONTENUES DANS LE MODELE
!      REMARQUE : LES GRANDEURS SONT LUES PAR LE SP
!
! IN  : MOTFAC  : MOT-CLE FACTEUR
! IN  : MODEL0  : NOM DE LA SD MODELE
!     ------------------------------------------------------------------
    implicit   none
!
! DECLARATION PARAMETRES D'APPELS
! -------------------------------
    include 'asterc/getvr8.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ltcrsd.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    character(len=*) :: motfac, model0
!     ------------------------------------------------------------------
!
!
! DECLARATION VARIABLES LOCALES
!
    integer :: nbpar
    parameter  ( nbpar = 2 )
    integer :: nbmcle
    parameter  ( nbmcle = 3 )
!     ------------------------------------------------------------------
    integer :: n1, ibid, iaux
    real(kind=8) :: vr
    complex(kind=8) :: cbid
    character(len=2) :: typpar(nbpar)
    character(len=8) :: nompar(nbpar)
    character(len=8) :: modele, vk
    character(len=8) :: nomgrd(nbmcle)
    character(len=16) :: motcle(nbmcle)
    character(len=19) :: table
    integer :: iarg
!     ------------------------------------------------------------------
    data motcle / 'LONGUEUR', 'PRESSION', 'TEMPERATURE' /
    data nomgrd / 'LONGUEUR', 'PRESSION', 'TEMP' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    modele = model0
!
!====
! 1. ACCROCHAGE D'UNE LISTE DE TABLE A LA STRUCTURE 'MODELE'
!====
!
! 1.1 ==> CREATION D'UNE LISTE DE TABLE
!         REMARQUE : LA LISTE NE CONTIENT QU'UNE SEULE TABLE !
!
    call ltcrsd(modele, 'G')
!
! 1.2 ==> RECUPERATION DU NOM DE LA TABLE DANS LA LISTE
!
    call ltnotb(modele, 'CARA_ETUDE', table)
!
!====
! 2. REMPLISSAGE DE LA TABLE
!====
!
! 2.1. ==> CREATION D'UNE TABLE CONTENANT LES GRANDEURS CARACTERISTIQUES
!
    call tbcrsd(table, 'G')
!
    nompar(1)='GRANDEUR'
    typpar(1)='K8'
    nompar(2)='VALE'
    typpar(2)='R'
    call tbajpa(table, nbpar, nompar, typpar)
!
! 2.2. ==> LECTURE ET STOCKAGE DES VALEURS PRESENTES
!
    do 10 , iaux = 1 , nbmcle
!
    call getvr8(motfac, motcle(iaux), 1, iarg, 1,&
                vr, n1)
    if (n1 .gt. 0) then
        vk = nomgrd(iaux)
        call tbajli(table, nbpar, nompar, ibid, vr,&
                    cbid, vk, 0)
    endif
!
    10 end do
!
    call jedema()
!
end subroutine
