subroutine xcgfvo(option, ndim, nnop, fno, rho)
!
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecach.h'
    include 'asterfort/vecini.h'
    character(len=16) :: option
    integer :: ndim, nnop
    real(kind=8) :: fno(ndim*nnop), rho
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRS_1404
!
!    BUT : CALCUL DES CHARGES VOLUMIQUES AUX NOEUD DE L'ELEM PARENT
!         POUR LES OPTIONS CALC_G, CALC_G_F, CALC_K_G ET CALC_K_G_F
!
!
! IN  OPTION : OPTION DE CALCUL
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! OUT FNO    : FORCES NODALES CORRESPONDANT AUX CHARGES VOLUMIQUES
! OUT RHO    : MASSE VOLUMIQUE
!
!
!
    integer :: igeom, imate, iforc, iforf, itemps, ipesa, irota
    integer :: iret, ino, j, kk, mxstac
    logical :: fonc
    real(kind=8) :: valpar(4), rbid, om, omo
    integer :: icodre
    character(len=8) :: nompar(4)
    character(len=16) :: phenom
    parameter   (mxstac=1000)
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    call assert(ndim*nnop.le.mxstac)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
!     PARAMETRES DES FORCES VOLUMIQUES
    if (option .eq. 'CALC_G' .or. option .eq. 'CALC_K_G') then
        fonc=.false.
        call jevech('PFRVOLU', 'L', iforc)
    else if (option.eq.'CALC_G_F'.or. option.eq.'CALC_K_G_F') then
        fonc=.true.
        call jevech('PFFVOLU', 'L', iforf)
        call jevech('PTEMPSR', 'L', itemps)
    else
        call assert(.false.)
    endif
!
    call tecach('ONN', 'PPESANR', 'L', 1, ipesa,&
                iret)
    call tecach('ONN', 'PROTATR', 'L', 1, irota,&
                iret)
!
!     INITIALISATION DE FNO
    call vecini(ndim*nnop, 0.d0, fno)
!
!     INITIALISATION DE RHO
    rho = 0.d0
!
!     ------------------------------------------------------------------
!                     TRAITEMENT DES FORCES VOLUMIQUES
!     ------------------------------------------------------------------
!
!     FORCES VOLUMIQUES FONCTION
    if (fonc) then
!
        nompar(1) = 'X'
        nompar(2) = 'Y'
        valpar(ndim+1) = zr(itemps)
        if (ndim .eq. 2) then
            nompar(3) = 'INST'
        else if (ndim.eq.3) then
            nompar(3) = 'Z'
            nompar(4) = 'INST'
        endif
!
!       INTERPOLATION DE LA FORCE (FONCTION PAR ELEMENT) AUX NOEUDS
        do 30 ino = 1, nnop
            do 31 j = 1, ndim
                valpar(j) = zr(igeom+ndim*(ino-1)+j-1)
31          continue
            do 32 j = 1, ndim
                kk = ndim*(ino-1)+j
                call fointe('FM', zk8(iforf+j-1), ndim+1, nompar, valpar,&
                            fno(kk), iret)
32          continue
30      continue
!
!     FORCES VOLUMIQUES CONSTANTES (AUX NOEUDS)
    else
!
        do 33 ino = 1, nnop
            do 34 j = 1, ndim
                fno(ndim*(ino-1)+j) = zr(iforc+ndim*(ino-1)+j-1)
34          continue
33      continue
!
    endif
!
!     ------------------------------------------------------------------
!            TRAITEMENT DES FORCES DE PESANTEUR OU DE ROTATION
!     ------------------------------------------------------------------
!
    if ((ipesa.ne.0) .or. (irota.ne.0)) then
!
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
        call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, ' ', rbid,&
                    1, 'RHO', rho, icodre, 1)
!
        if (ipesa .ne. 0) then
            do 60 ino = 1, nnop
                do 61 j = 1, ndim
                    kk = ndim*(ino-1)+j
                    fno(kk) = fno(kk) + rho*zr(ipesa)*zr(ipesa+j)
61              continue
60          continue
        endif
!
        if (irota .ne. 0) then
            om = zr(irota)
            do 62 ino = 1, nnop
                omo = 0.d0
                do 63 j = 1, ndim
                    omo = omo + zr(irota+j)* zr(igeom+ndim*(ino-1)+j- 1)
63              continue
                do 64 j = 1, ndim
                    kk = ndim*(ino-1)+j
                    fno(kk)=fno(kk)+rho*om*om*(zr(igeom+kk-1)-omo*zr(&
                    irota+j))
64              continue
62          continue
        endif
!
    endif
!
!     ------------------------------------------------------------------
!
end subroutine
