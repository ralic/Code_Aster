subroutine prfcur(vec1, nbn, vec2, nbp, interp,&
                  prolgd)
    implicit none
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     PROLONGEMENT DE LA FONCTION SUIVANT OPTION CHOISIE
!     ------------------------------------------------------------------
!     IN  : VEC1   : POINTEUR DE NOEUDS DANS LA LISTE DE NOEUDS
!     IN  : NBN    : DIMENSION DU VECTEUR VEC1
!     I/O : VEC2   : VALEURS DE LA FONCTION
!     IN  : NBP    : DIMENSION DU VECTEUR VEC2
!     IN  : INTERP : TYPE INTERPOLATION DE LA FONCTION
!     IN  : PROLGD : TYPE DE PROLONGEMENT DE LA FONCTION
    include 'jeveux.h'
    integer :: vec1(nbn)
    real(kind=8) :: vec2(nbp)
    character(len=2) :: prolgd
    character(len=8) :: interp
    integer :: i, id, ide, if, ifi, ip, j
    integer :: nbn, nbp, nbp2
    real(kind=8) :: resu, x, x1, x2, y1, y2, xline, xlog
!-----------------------------------------------------------------------
    xline(x,x1,y1,x2,y2) = y1+(x-x1)*(y2-y1)/(x2-x1)
    xlog(x,x1,x2,y1,y2)=exp(log(y1)+(log(x)-log(x1))*(log(y2)-log(y1))&
     &                                /(log(x2)-log(x1)))
!     ------------------------------------------------------------------
!
!     --- INTERPOLATION DE LA FONCTION ---
!
    do 10 i = 1, nbn-1
        ide = vec1(i)
        ifi = vec1(i+1)
        if (ide+1 .ne. ifi) then
            ip = (ifi-ide)-1
            do 20 j = 1, ip
                if (interp(1:3) .eq. 'LIN') then
                    vec2(2*ide+2*j) = xline(&
                                      vec2(2*ide+2*j-1), vec2(2* ide-1), vec2(2*ide),&
                                      vec2(2*ifi-1), vec2(2*ifi)&
                                      )
                else if (interp(1:3) .eq. 'LOG') then
                    vec2(2*ide+2*j) = xlog(&
                                      vec2(2*ide+2*j-1), vec2(2* ide-1), vec2(2*ide),&
                                      vec2(2*ifi-1), vec2(2*ifi)&
                                      )
                endif
20          continue
        endif
10  end do
!
!     --- PROLONGEMENT A GAUCHE ---
!
    id = vec1(1)
    if (prolgd(1:1) .eq. 'C') then
        do 30 i = 1, id-1
            vec2(2*i) = vec2(2*id-1)
30      continue
    else if (prolgd(1:1) .eq. 'L') then
        do 40 i = 1, id-1
            resu = xline(vec2(2*i-1), vec2(2*id-1), vec2(2*id), vec2(2*( id+1)-1), vec2(2*(id+1))&
                   )
            if (resu .lt. 0.d0) then
                resu = 0.d0
            endif
            vec2(2*i) = resu
40      continue
    endif
!
!     --- PROLONGEMENT A DROITE ---
!
    nbp2 = nbp/2
    if = vec1(nbn)
    if (prolgd(2:2) .eq. 'C') then
        do 50 i = if+1, nbp2
            vec2(2*i) = vec2(2*if)
50      continue
    else if (prolgd(2:2) .eq. 'L') then
        do 60 i = if+1, nbp2
            resu = xline(vec2(2*i-1), vec2(2*if-1), vec2(2*if), vec2(2*( if-1)-1), vec2(2*(if-1))&
                   )
            if (resu .lt. 0.d0) then
                resu = 0.d0
            endif
            vec2(i) = resu
60      continue
    endif
end subroutine
