function typele(ligrez, igrel)
    implicit none
    integer :: typele
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=*) :: ligrez
    integer :: igrel
! ----------------------------------------------------------------------
!     ENTREES:
!       LIGREL : NOM D'1 LIGREL
!       IGREL  : NUMERO D'1 GREL
!
!     SORTIES:
!       TYPELE : TYPE_ELEMENT ASSOCIE AU GREL
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=19) :: ligrel
    integer :: liel, n1
    character(len=1) :: k1bid
! ----------------------------------------------------------------------
!     COMMONS DE CALCUL.F :
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
! ----------------------------------------------------------------------
    ligrel = ligrez
!
!     -- SI ON EST "SOUS" CALCUL, ON PEUT ALLER PLUS VITE :
    if (iactif .eq. 1) then
        n1=zi(illiel-1+igrel+1)-zi(illiel-1+igrel)
        typele=zi(ialiel-1+zi(illiel-1+igrel)-1+n1)
    else
        call jemarq()
        call jeveuo(jexnum(ligrel//'.LIEL', igrel), 'L', liel)
        call jelira(jexnum(ligrel//'.LIEL', igrel), 'LONMAX', n1, k1bid)
        typele = zi(liel-1+n1)
        call jedema()
    endif
end function
