subroutine xffext(jinfo, nfon, nmafon, listpt, ptextr,&
                  nbptex)
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    integer :: jinfo, nfon, nmafon, nbptex
    character(len=19) :: listpt, ptextr
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
!              RECHERCHE DES POINTS EXTREMITES DU FOND DE FISSURE
!
! ----------------------------------------------------------------------
!
!
! IN  JINFO      : ADRESSE DU VECTEUR INFO DE LA SD
! IN  NFON       : NOMBRE DE POINTS AU FOND DE FISSURE
! IN  NMAFON     : NOMBRE DE MAILLES CONNECTEES AU FOND
! IN  LISTPT     : VECTEUR CONTENANT LES INDICES DES POINTS DU FOND PAR
!                  MAILLE
!
! IN/OUT PTEXTR  : VECTEUR CONTENANT LES INDICES DES POINTS EXTREMITES
!                  DU OU DES FONDS DE FISSURE
! OUT    NBPTEX  : NOMBRE D'EXTREMITES DU OU DES FONDS
!
!
    integer :: ima, ipt, jlistp, jptext, nocc, ptasso
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call wkvect(ptextr, 'V V I', nfon, jptext)
!
    call jeveuo(listpt, 'L', jlistp)
!
    nbptex = 0
!
    do 10 ipt = 1, nfon
!       COMPTEUR DU NOMBRE D'OCCURENCES DU POINT IPT DANS LE TABLEAU
!       LISTPT
        nocc = 0
!       INDICE DU POINT ASSOCIE AU POINT IPT
        ptasso = 0
!
        do 11 ima = 1, nmafon
            if ((zi(jlistp-1+2*(ima-1)+1).eq.ipt) .and. (zi(jlistp-1+ 2*(ima-1)+2).ne.0)&
                .and. (ptasso.ne.zi(jlistp-1+2*(ima-1)+ 2) )) then
!
                ptasso = zi(jlistp-1+2*(ima-1)+2)
                nocc = nocc + 1
!
                elseif ((zi(jlistp-1+2*(ima-1)+2).eq.ipt) .and. (&
            ptasso.ne.zi(jlistp-1+2*(ima-1)+1) )) then
!
                ptasso = zi(jlistp-1+2*(ima-1)+1)
                nocc = nocc + 1
            endif
!
            if (nocc .eq. 2) goto 10
!
11      continue
!
!       UNE SEULE OCCURENCE DU POINT IPT:
!       C'EST UN POINT EXTREMITE DU FOND DE FISSURE
        if (nocc .eq. 1) then
            nbptex = nbptex + 1
            zi(jptext-1+nbptex) = ipt
        endif
10  end do
!
!     ON DOIT AVOIR UN NOMBRE PAIR D'EXTREMITES
    call assert(mod(nbptex, 2).eq.0)
!
    zk16(jinfo-1+3) = 'OUVERT'
!     CAS D'UN FOND FERME SI ABSCENCE D'EXTREMITES
    if (nbptex .eq. 0) then
        zk16(jinfo-1+3) = 'FERME'
        zi(jptext-1+1) = 1
    endif
!
    call jedema()
end subroutine
