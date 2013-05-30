subroutine rc32fs(nbsigr, nocc, situ, fuijs, fuij,&
                  fuse, ns, nscy, ug)
    implicit   none
    include 'asterfort/infniv.h'
    integer :: nbsigr, nocc(*), situ(*), ns, nscy
    real(kind=8) :: fuijs(*), fuij(*), fuse, ug
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     CALCUL DU FACTEUR D'USAGE
!
!     ------------------------------------------------------------------
    integer :: is1, is2, i, i1, ifm, k, l, niv, ns2, icomp
    real(kind=8) :: salt, fum, u1kl, u2kl
    logical :: trouve
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE FACTEUR D USAGE INITIALE (AVEC SEISME)'
        write(ifm,1012) ( situ(l),l=1,nbsigr )
        write(ifm,1010) ( nocc(l),l=1,nbsigr )
        do 100 i = 1, nbsigr
            i1 = nbsigr*(i-1)
            write(ifm,1000) situ(i), nocc(i), (fuijs(i1+l),l=1,nbsigr)
100      continue
    endif
!
    ug = 0.d0
    ns2 = ns / 2
    icomp = 0
!
10  continue
    fum = 0.d0
    trouve = .false.
    icomp = icomp + 1
    if (icomp .gt. ns2) goto 9999
!
! --- ON SELECTIONNE LES 'NS2' COMBINAISONS LES PLUS PENALISANTES
!     SANS PRENDRE EN COMPTE LE SEISME (MATRICE FUIJ)
!
    do 20 k = 1, nbsigr
!
        do 22 l = 1, nbsigr
!
            salt = fuij(nbsigr*(k-1)+l)
!
            if (salt .gt. fum) then
                is1 = k
                is2 = l
                fum = salt
                trouve = .true.
            endif
!
22      continue
!
20  end do
!
    if (trouve) then
!
! ------ ON RECUPERE LA VALEUR ASSOCIEE AVEC PRISE EN COMPTE DU SEISME
!        (MATRICE FUIJS)
!
        fum = fuijs(nbsigr*(is1-1)+is2)
!
        u1kl = fum
        u2kl = dble( 2*nscy-1 ) * fuse
!
        if (niv .ge. 2) then
            write(ifm,1040)'=> FU MAXI (SANS SEISME) = ', fuij(nbsigr*&
            (is1-1)+is2), situ(is1), situ(is2)
            write(ifm,1020)'        U1KL = ', u1kl
            write(ifm,1020)'        U2KL = ', u2kl
        endif
!
        fuij(nbsigr*(is1-1)+is2) = 0.d0
        fuij(nbsigr*(is2-1)+is1) = 0.d0
! POUR L IMPRESSION ON MODIFIE AUSSI FUIJS
        fuijs(nbsigr*(is1-1)+is2) = 0.d0
        fuijs(nbsigr*(is2-1)+is1) = 0.d0
!
!
        if (niv .ge. 2) then
            write(ifm,*) 'MATRICE FACTEUR D USAGE MODIFIEE (AVEC SEISME)'
            write(ifm,1012) ( situ(l),l=1,nbsigr )
            write(ifm,1010) ( nocc(l),l=1,nbsigr )
            do 110 i = 1, nbsigr
                i1 = nbsigr*(i-1)
                write(ifm,1000) situ(i), nocc(i), (fuijs(i1+l),l=1,&
                nbsigr)
110          continue
        endif
!
        ug = ug + u1kl + u2kl
        goto 10
!
    endif
!
9999  continue
!
    1000 format(1p,i7,i9,'|',40(e9.2,'|'))
    1010 format(1p,7x,'NB_OCCUR ','|',40(i9,'|'))
    1012 format(1p,7x,'SITUATION','|',40(i9,'|'))
    1040 format(1p,a30,e12.5,', LIGNE:',i4,', COLONNE:',i4)
    1020 format(1p,a15,e12.5)
!
end subroutine
