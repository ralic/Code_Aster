subroutine rcvala(jmat, nomat, phenom, nbpar, nompar,&
                  valpar, nbres, nomres, valres, icodre,&
                  iarret)
    implicit none
    include 'jeveux.h'
    include 'asterfort/fointa.h'
    include 'asterfort/rcvals.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    integer :: imat, nbpar, nbres, iarret
    real(kind=8) :: valpar(nbpar), valres(nbres)
    integer :: icodre(nbres)
    character(len=*) :: nomat, phenom, nompar(nbpar), nomres(nbres)
! ----------------------------------------------------------------------
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
!
!     OBTENTION DE LA VALEUR VALRES D'UN "ELEMENT" D'UNE RELATION DE
!     COMPORTEMENT D'UN MATERIAU DONNE
!
!     ARGUMENTS D'ENTREE:
!        JMAT   : ADRESSE DE LA LISTE DE MATERIAU CODE
!        NOMAT  : NOM DU MATERIAU DANS LE CAS D'UNE LISTE DE MATERIAU
!                 SI = ' ', ON EXPLOITE LE PREMIER DE LA LISTE
!        PHENOM : NOM DU PHENOMENE
!        NBPAR  : NOMBRE DE PARAMETRES DANS NOMPAR ET VALPAR
!        NOMPAR : NOMS DES PARAMETRES(EX: TEMPERATURE )
!        VALPAR : VALEURS DES PARAMETRES
!        NBRES  : NOMBRE DE RESULTATS
!        NOMRES : NOM DES RESULTATS (EX: E,NU,... )
!                 TELS QU'IL FIGURENT DANS LA COMMANDE MATERIAU
!       IARRET = 0 : ON REMPLIT ICODRE ET ON SORT SANS MESSAGE.
!              = 1 : SI UN DES PARAMETRES N'EST PAS TROUVE, ON ARRETE
!                       EN FATAL EN INDIQUANT LE NOM DE LA MAILLE.
!              = 2 : IDEM QUE 1 MAIS ON N'INDIQUE PAS LA MAILLE.
!
!     ARGUMENTS DE SORTIE:
!     VALRES : VALEURS DES RESULTATS APRES RECUPERATION ET INTERPOLATION
!     ICODRE : POUR CHAQUE RESULTAT, 0 SI ON A TROUVE, 1 SINON
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
! --- PARAMETER ASSOCIE AU MATERIAU CODE
    integer :: lmat, lfct, lsup
    parameter  ( lmat = 7 , lfct = 9 , lsup = 2 )
!
    integer :: ires, icomp, ipi, iadzi, iazk24, nbobj, nbr, nbc, nbf, ivalk
    integer :: ivalr, ir, ipif, ik, nbmat, jmat, kmat, inom
    character(len=8) :: nomail, nomi
    character(len=10) :: nomphe
    character(len=24) :: valk(2)
! DEB ------------------------------------------------------------------
!
!     -- ON EST OBLIGE DE RECOPIER PHENOM CAR IL FAUT LE TRONQUER
!        PARFOIS A 10 AVANT DE LE COMPARER
    nomphe=phenom
!
!  ON EXPLORE L'ENTETE DE LA SD MATER_CODE POUR DETERMINER LE MATERIAU
!  DE LA LA LISTE
!
    nbmat=zi(jmat)
    if (nomat(1:1) .ne. ' ') then
        do 5 kmat = 1, nbmat
            inom=zi(jmat+kmat)
            nomi=zk8(inom)
            if (nomi .eq. nomat) then
                imat = jmat+zi(jmat+nbmat+kmat)
                goto 9
            endif
 5      continue
        call u2mesk('F', 'MODELISA6_92', 1, nomat)
    else
        imat = jmat+zi(jmat+nbmat+1)
    endif
!
 9  continue
!
!  ON TRAITE LE MATERIAU SELECTIONNE DANS LA LISTE
!
    do 10 ires = 1, nbres
        icodre(ires) = 1
10  end do
    do 20 icomp = 1, zi(imat+1)
        if (nomphe .eq. zk16(zi(imat)+icomp-1)(1:10)) then
            ipi = zi(imat+2+icomp-1)
            goto 22
        endif
20  end do
!
! --- SELON LA VALEUR DE IARRET ON ARRETE OU NON :
    if (iarret .ge. 1) then
        valk(1)=nomphe
        if (iarret .eq. 1) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            valk(2) = nomail
            call u2mesk('F', 'MODELISA9_75', 2, valk)
        else
            call u2mesk('F', 'MODELISA9_74', 1, valk)
        endif
    endif
    goto 999
!
22  continue
!
    nbobj = 0
    nbr = zi(ipi)
    nbc = zi(ipi+1)
    nbf = zi(ipi+2)
    ivalk = zi(ipi+3)
    ivalr = zi(ipi+4)
    do 32 ires = 1, nbres
        do 30 ir = 1, nbr
            if (nomres(ires) .eq. zk8(ivalk+ir-1)) then
                valres(ires) = zr(ivalr-1+ir)
                icodre(ires) = 0
                nbobj = nbobj + 1
                goto 32
            endif
30      continue
32  end do
!
    if (nbobj .ne. nbres) then
        do 40 ires = 1, nbres
            ipif = ipi+lmat-1
            do 42 ik = 1, nbf
                if (nomres(ires) .eq. zk8(ivalk+nbr+nbc+ik-1)) then
                    call fointa(ipif, nbpar, nompar, valpar, valres(ires))
                    icodre(ires) = 0
                endif
                ipif = ipif + lfct
                if (nomphe(1:8) .eq. 'TRACTION') then
                    ipif = ipif + lsup
                else if (nomphe.eq. 'META_TRACT') then
                    ipif = ipif + lsup
                endif
42          continue
40      continue
    endif
!
999  continue
!
    call rcvals(iarret, icodre, nbres, nomres)
!
end subroutine
