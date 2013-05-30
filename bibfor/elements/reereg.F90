subroutine reereg(stop, elrefp, nnop, coor, xg,&
                  ndim, xe, iret)
!
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
! TOLE CRS_1404
!
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/elrfdf.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/invjax.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'blas/ddot.h'
    character(len=1) :: stop
    character(len=8) :: elrefp
    integer :: nnop, ndim
    real(kind=8) :: coor(ndim*nnop)
    real(kind=8) :: xg(ndim)
    real(kind=8) :: xe(ndim)
    integer :: iret
!
! ----------------------------------------------------------------------
!
! TROUVER LES COORDONNEES DANS L'ELEMENT DE REFERENCE D'UN
! POINT DONNE DANS L'ESPACE REEL PAR LA METHODE DE NEWTON
!
! ----------------------------------------------------------------------
!
!
! IN  STOP   : /'S' : ON S'ARRETE EN ERREUR <F> EN CAS D'ECHEC
!              /'C' : ON CONTINUE EN CAS D'ECHEC (IRET=1)
! IN  ELREFP : TYPE DE L'ELEMENT
! IN  NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  COOR   : COORDONNEES DS ESPACE REEL DES NOEUDS DE L'ELEMENT
! IN  XG     : COORDONNEES DU POINT DANS L'ESPACE REEL
! IN  NDIM   : DIMENSION DE L'ESPACE
! OUT XE     : COORDONNEES DU POINT DANS L'ESPACE PARA DE L'ELEMENT
! OUT IRET   : 0 : ON A CONVERGE
!            : 1 : ON N'A PAS CONVERGE
!
! ----------------------------------------------------------------------
!
    integer :: nbnomx, itermx
    real(kind=8) :: toler
    parameter   (nbnomx = 27 , toler = 1.d-8 , itermx = 50)
!
    real(kind=8) :: zero
    integer :: iter, i, k, idim, ino, ipb
    integer :: nno, nderiv
    real(kind=8) :: etmp(3), err
    real(kind=8) :: point(ndim), xenew(ndim), invjac(3, 3)
    real(kind=8) :: dff(3, nbnomx)
    real(kind=8) :: ff(nnop)
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    zero = 0.d0
    iter = 0
    iret = 0
    call vecini(ndim, zero, xe)
!
100  continue
    iter=iter+1
!
! --- VALEURS DES FONCTIONS DE FORME EN XE: FF
!
    call elrfvf(elrefp, xe, nbnomx, ff, nno)
    call assert(nno.eq.nnop)
!
! --- DERIVEES PREMIERES DES FONCTIONS DE FORME EN XE: DFF
!
    call elrfdf(elrefp, xe, ndim*nbnomx, dff, nno,&
                nderiv)
!      CALL ASSERT(NDERIV.EQ.NDIM)
!
! --- CALCUL DES COORDONNEES DU POINT: POINT
!
    call vecini(ndim, zero, point)
    do 200 idim = 1, ndim
        do 210 ino = 1, nno
            point(idim) = point(idim)+ff(ino)*coor(ndim*(ino-1)+idim)
210      continue
200  end do
!
! --- CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE: INVJAC
!
    call invjax(stop, nno, ndim, nderiv, dff,&
                coor, invjac, ipb)
    if (ipb .eq. 1) then
        if (stop .eq. 'S') then
            call u2mess('F', 'ALGORITH5_19')
        else if (stop.eq.'C') then
            iret=1
            goto 999
        else
            call assert(.false.)
        endif
    endif
!
! --- UPDATE XE
!
    do 220 i = 1, ndim
        xenew(i)=xe(i)
        do 230 k = 1, ndim
            xenew(i) = xenew(i)-invjac(i,k)*(point(k)-xg(k))
230      continue
220  end do
!
! --- CALCUL DE L'ERREUR: ERR
!
    do 240 i = 1, ndim
        etmp(i) = xenew(i) - xe(i)
240  end do
    err = ddot(nderiv,etmp,1,etmp,1)
!
! --- NOUVELLE VALEUR DE XE
!
    call lceqvn(ndim, xenew, xe)
!
! --- TEST DE FIN DE BOUCLE
!
    if (err .le. toler) then
        goto 999
    else if (iter.lt.itermx) then
        goto 100
    else
        if (stop .eq. 'S') then
            call u2mess('F', 'ELEMENTS2_58')
        else
            call assert(stop.eq.'C')
            iret=1
        endif
    endif
!
999  continue
!
end subroutine
