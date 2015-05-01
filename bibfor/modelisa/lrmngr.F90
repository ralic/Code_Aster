subroutine lrmngr(ngrmax, ngrp, numgrp, nomgrp, jnogrp,&
                  jlggrp, nbnufa, nomjng, nomjlg)
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!     CREATION DES VECTEURS NOMS DE GROUPES DE MAILLES ET DE NOEUDS
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
    integer :: ngrp, numgrp, jnogrp, jlggrp, ngrmax, nbnufa
    character(len=*) :: nomjng, nomjlg
    character(len=24) :: nomgrp
    integer :: i, num
!     ------------------------------------------------------------------
!     SI CE NOM DE GROUPE EXISTE DEJA ON NE LE RESTOCKE PAS MAIS
!     ON INCREMENTE DU NB NOEUDS/MAILLES DE LA FAMILLE OU IL APPARAIT
    do 1 i = 1, ngrp
        if (zk24(jnogrp+i-1) .eq. nomgrp) then
            zi (jlggrp+i-1) = zi(jlggrp+i-1) + nbnufa
            goto 100
        endif
 1  end do
!     SOIT ON NUMEROTE A LA SUITE NGRP
    if (numgrp .eq. 99999999) then
        ngrp = ngrp + 1
        num = ngrp
!     SOIT ON UTILISE CE NUMERO DE NOEUD NUMGRP
!     ET ON MEMORISE LE PLUS GRAND NUMERO ATTEINT DANS NGRP
    else
        if (numgrp .gt. ngrp) ngrp = numgrp
        num = numgrp
    endif
!     ON AGRANDI NOMJNG + NOMJLG SI NECESSAIRE
    if (num .gt. ngrmax) then
        ngrmax = nint(num * 1.5d0)
        call juveca(nomjng, ngrmax)
        call jeveuo(nomjng, 'E', jnogrp)
        call juveca(nomjlg, ngrmax)
        call jeveuo(nomjlg, 'E', jlggrp)
    endif
    zk24(jnogrp+num-1) = nomgrp
    zi (jlggrp+num-1) = nbnufa
100  continue
end subroutine
