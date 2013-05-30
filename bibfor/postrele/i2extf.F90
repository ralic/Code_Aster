subroutine i2extf(m, f, conec, type, n1,&
                  n2)
    implicit none
!
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
!**********************************************************************
!
!     SAISIE DES EXTREMITES D' UNE FACE D' UNE MAILLE EN 2D
!
!       M     (IN)  : NUMERO DE LA MAILLE
!
!       F     (IN)  : NUMERO DE LA FACE
!
!       CONEC (IN)  : NOM DE L' OBJET CONECTIVITE
!
!       TYPE  (IN)  : NOM DE L' OBJET CONTENANT LES TYPES DES MAILLES
!
!       N1    (OUT) : NUMERO DU NOEUD ORIGINE
!
!       N2    (OUT) : NUMERO DU NOEUD EXTREMITE
!
!**********************************************************************
!
    include 'jeveux.h'
!
    include 'asterfort/i2nbrf.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=*) :: conec
    character(len=*) :: type
    integer :: m, f, n1, n2
!
    integer :: adrm, atypm, nbn, nbf
    character(len=8) :: typm
!
!
!
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
    integer :: iatyma
!-----------------------------------------------------------------------
    call jemarq()
    nbn = 0
    nbf = 0
    adrm = 0
    atypm = 0
    typm = ' '
!
    call jeveuo(jexnum(conec, m), 'L', adrm)
    call jeveuo(type, 'L', iatyma)
    atypm=iatyma-1+m
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(atypm)), typm)
!
    if ((typm .eq. 'SEG2') .or. (typm .eq. 'SEG3')) then
!
        n1 = zi(adrm)
        n2 = zi(adrm + 1)
!
    else
!
        call jelira(jexnum(conec, m), 'LONMAX', nbn, k1bid)
        call i2nbrf(nbn, nbf)
!
        n1 = zi(adrm + f-1)
!
        if (f .eq. nbf) then
!
            n2 = zi(adrm)
!
        else
!
            n2 = zi(adrm + f)
!
        endif
!
    endif
!
    call jedema()
end subroutine
