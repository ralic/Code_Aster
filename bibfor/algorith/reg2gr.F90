subroutine reg2gr(imate, compor, ndim, regula, dimdef,&
                  defgep, sigp, dsde2g)
! ======================================================================
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- BUT : CALCUL DE LA LOI DE COMPORTEMENT ELASTIQUE POUR LA PARTIE --
! ---       SECOND GRADIENT --------------------------------------------
! ======================================================================
    implicit      none
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mesk.h'
    integer :: imate, ndim, dimdef, regula(6)
    real(kind=8) :: sigp(ndim*ndim*ndim), defgep(dimdef)
    real(kind=8) :: dsde2g(ndim*ndim*ndim, ndim*ndim*ndim)
    character(len=16) :: compor(*)
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: p, q, r, l, m, n, adder2, kpg, spt
    real(kind=8) :: val(5), id(ndim, ndim)
    integer :: icodre(5)
    character(len=8) :: ncra(5), fami, poum
! ======================================================================
! --- DEFINITION DES DONNEES INITIALES ---------------------------------
! ======================================================================
    data ncra  / 'A1','A2','A3','A4','A5' /
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    if (compor(1) .eq. 'ELAS') then
        do 10 p = 1, ndim*ndim*ndim
            do 20 q = 1, ndim*ndim*ndim
                dsde2g(q,p)=0.0d0
20          continue
10      continue
        do 30 p = 1, ndim
            do 40 q = 1, ndim
                id(q,p)=0.0d0
40          continue
            id(p,p)=1.0d0
30      continue
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', 'ELAS_2NDG', 0, ' ', 0.0d0,&
                    5, ncra(1), val(1), icodre(1), 1)
!
        do 50 p = 1, ndim
            do 60 q = 1, ndim
                do 70 r = 1, ndim
                    do 80 l = 1, ndim
                        do 90 m = 1, ndim
                            do 100 n = 1, ndim
                                dsde2g((p-1)*ndim*ndim+(q-1)*ndim+r,&
                                (l-1)*ndim*ndim+(m-1)*ndim+n) =&
                                val(1)/2.0d0*(id(p,q)*(id(l,m)*id(r,n)&
                                +id(l,n)*id(r,m)) + id(p,r)*(id(l,m)*&
                                id(q,n)+id(l,n)*id(q,m)))+ val(2)/&
                                2.0d0*(id(p,q)*id(r,l)*id(m,n)&
                                + id(q,r)*(id(l,m)*id(p,n)+id(l,n)*id(&
                                p,m)) + id(p,r)*id(q,l)*id(m,n))&
                                + val(3)*2.0d0*id(r,q)*id(p,l)*id(m,n)&
                                + val(4)*id(p,l)*(id(q,m)*id(r,n)+id(&
                                q,n)*id(r,m)) + val(5)/2.0d0*(id(r,l)*&
                                (id(p,m)*id(q,n)+id(p,n)*id(q,m)) +&
                                id(q,l)*(id(r,m)*id(p,n)+id(r,n)*id(p,&
                                m)))
100                          continue
90                      continue
80                  continue
70              continue
60          continue
50      continue
        adder2 = regula(2)
!
        do 110 p = 1, ndim*ndim*ndim
            sigp(p)=0.0d0
            do 120 q = 1, ndim*ndim*ndim
                sigp(p)=sigp(p)+dsde2g(p,q)*defgep(adder2-1+q)
120          continue
110      continue
    else
        call u2mesk('F', 'ALGORITH4_50', 1, compor(1))
    endif
! ======================================================================
end subroutine
