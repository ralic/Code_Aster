subroutine rc36fu(nbsigr, nocc, situ, saltij, nommat,&
                  ug, factus)
    implicit   none
    include 'asterfort/infniv.h'
    include 'asterfort/limend.h'
    include 'asterfort/rc36f0.h'
    include 'asterfort/rc36f2.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/u2mesg.h'
    integer :: nbsigr, nocc(*), situ(*)
    real(kind=8) :: saltij(*), ug, factus(*)
    character(len=*) :: nommat
!     ------------------------------------------------------------------
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
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     CALCUL DU FACTEUR D'USAGE
!
!     ------------------------------------------------------------------
    integer :: isk, isl, k, l, nk, nl, n0, i1, i1a4, ifm, niv, icompt
    real(kind=8) :: saltm, nadm, ukl, vale(2)
    logical :: trouve, endur
    integer :: icodre
    character(len=2) :: k2c, k2l
    character(len=8) :: kbid
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE SALT INITIALE'
        write(ifm,1012) ( situ(2*(l-1)+1),situ(2*(l-1)+2),l=1,nbsigr )
        write(ifm,1010) ( nocc(2*(l-1)+1),nocc(2*(l-1)+2),l=1,nbsigr )
        do 100 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            write(ifm,1000) situ(2*(k-1)+1), nocc(2*(k-1)+1), (saltij(&
            i1+4*(l-1)+1),saltij(i1+4*(l-1)+3), l=1,nbsigr)
            write(ifm,1002) situ(2*(k-1)+2), nocc(2*(k-1)+2), (saltij(&
            i1+4*(l-1)+2),saltij(i1+4*(l-1)+4), l=1,nbsigr)
100      continue
    endif
!
    icompt = 0
    ug = 0.d0
!
10  continue
    saltm = 0.d0
    trouve = .false.
!
! --- RECHERCHE DU SALT MAXI
!
    call rc36f0(nbsigr, nocc, saltij, saltm, trouve,&
                isk, isl, i1a4, nk, nl)
!
    if (.not. trouve) goto 9999
!
    n0 = min ( nk , nl )
    call limend(nommat, saltm, 'WOHLER', kbid, endur)
    if (endur) then
        ukl = 0.d0
    else
        call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', saltm,&
                    1, 'WOHLER  ', nadm, icodre, 2)
        if (nadm .lt. 0) then
            vale(1) = saltm
            vale(2) = nadm
            call u2mesg('A', 'POSTRCCM_32', 0, ' ', 0,&
                        0, 2, vale)
        endif
        ukl = dble( n0 ) / nadm
    endif
!
    if (icompt .le. 49) then
        icompt = icompt + 1
        factus(4*(icompt-1)+1) = i1a4
        factus(4*(icompt-1)+2) = situ(2*(isk-1)+1)
        factus(4*(icompt-1)+3) = situ(2*(isl-1)+1)
        factus(4*(icompt-1)+4) = ukl
    endif
!
    if (niv .ge. 2) then
        if (i1a4 .eq. 1 .or. i1a4 .eq. 3) then
            k2l = '_A'
        else
            k2l = '_B'
        endif
        if (i1a4 .eq. 1 .or. i1a4 .eq. 2) then
            k2c = '_A'
        else
            k2c = '_B'
        endif
        write(ifm,1040)'=> SALT MAXI = ', saltm, situ(2*(isk-1)+1),&
        k2l, situ(2*(isl-1)+1), k2c
        write(ifm,1030)'          N0 = ', n0
        write(ifm,1020)'        NADM = ', nadm
        write(ifm,1020)'         UKL = ', ukl
    endif
!
! --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT SUIVANT
!     LE NOMBRE D'OCCURENCE EGAL A ZERO
!
    call rc36f2(nbsigr, nocc, saltij, i1a4, isk,&
                isl, nk, nl, n0)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE SALT MODIFIEE'
        write(ifm,1012) ( situ(2*(l-1)+1),situ(2*(l-1)+2),l=1,nbsigr )
        write(ifm,1010) ( nocc(2*(l-1)+1),nocc(2*(l-1)+2),l=1,nbsigr )
        do 110 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            write(ifm,1000) situ(2*(k-1)+1), nocc(2*(k-1)+1), (saltij(&
            i1+4*(l-1)+1),saltij(i1+4*(l-1)+3), l=1,nbsigr)
            write(ifm,1002) situ(2*(k-1)+2), nocc(2*(k-1)+2), (saltij(&
            i1+4*(l-1)+2),saltij(i1+4*(l-1)+4), l=1,nbsigr)
110      continue
    endif
!
    ug = ug + ukl
    goto 10
!
9999  continue
!
    1000 format(1p,i7,'_A',i9,'|',40(e9.2,1x,e9.2,'|'))
    1002 format(1p,i7,'_B',i9,'|',40(e9.2,1x,e9.2,'|'))
    1010 format(1p,9x,'NB_OCCUR ','|',40(i9,1x,i9,'|'))
    1012 format(1p,9x,'SITUATION','|',40(i7,'_A',1x,i7,'_B|'))
    1040 format(1p,a15,e12.5,', LIGNE:',i4,a2,', COLONNE:',i4,a2)
! 1040 FORMAT(1P,A15,E12.5,' LIGNE ',I4,' COLONNE ',I4,' INDICE ',I4)
    1030 format(1p,a15,i12)
    1020 format(1p,a15,e12.5)
!
end subroutine
