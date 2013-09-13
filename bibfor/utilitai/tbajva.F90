subroutine tbajva(table, nbpara, nompar, vi, livi,&
                  vr, livr, vc, livc, vk,&
                  livk)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: nbpara, vi, livi(*)
    real(kind=8) :: vr, livr(*)
    complex(kind=8) :: vc, livc(*)
    character(len=*) :: table, nompar, vk, livk(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!      AJOUTER UNE LIGNE A UNE TABLE.
! ----------------------------------------------------------------------
! IN  : TABLE  : NOM DE LA STRUCTURE "TABLE".
! IN  : NBPARA : NOMBRE DE PARAMETRES DE NOMPAR
! IN  : NOMPAR : PARAMETRE POUR LEQUEL ON VEUT ECRIRE
! IN  : VI     : VALEUR POUR LE PARAMETRE "I"
! I/O : LIVI   : LISTE DES VALEURS POUR LES PARAMETRES "I"
! IN  : VR     : VALEUR POUR LE PARAMETRE "R"
! I/O : LIVR   : LISTE DES VALEURS POUR LES PARAMETRES "R"
! IN  : VC     : VALEUR POUR LE PARAMETRE "C"
! I/O : LIVC   : LISTE DES VALEURS POUR LES PARAMETRES "C"
! IN  : VK     : VALEUR POUR LE PARAMETRE "K"
! I/O : LIVK   : LISTE DES VALEURS POUR LES PARAMETRES "K"
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
!
    integer :: iret, nbcol, jtbnp
    integer :: jtblp, i, ki, kr, kc, kk
    character(len=19) :: nomtab
    character(len=24) :: type, nomcol
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = ' '
    nomtab = table
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call utmess('F', 'UTILITAI4_64')
    endif
    if (nomtab(18:19) .ne. '  ') then
        call utmess('F', 'UTILITAI4_68')
    endif
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
    nbcol = zi(jtbnp )
    ASSERT(nbcol.ne.0)
    ASSERT(nbcol.eq.nbpara)
!
    ki = 0
    kr = 0
    kc = 0
    kk = 0
    do 10 i = 1, nbcol
        nomcol = zk24(jtblp-1+4*(i-1)+1)
        type = zk24(jtblp-1+4*(i-1)+2)
        if (type(1:1) .eq. 'I') then
            ki = ki + 1
            if (nompar .eq. nomcol) then
                livi(ki) = vi
                goto 20
            endif
        else if (type(1:1).eq.'R') then
            kr = kr + 1
            if (nompar .eq. nomcol) then
                livr(kr) = vr
                goto 20
            endif
        else if (type(1:1).eq.'C') then
            kc = kc + 1
            if (nompar .eq. nomcol) then
                livc(kc) = vc
                goto 20
            endif
        else if (type(1:1).eq.'K') then
            kk = kk + 1
            if (nompar .eq. nomcol) then
                livk(kk) = vk
                goto 20
            endif
        endif
10  continue
    call utmess('F', 'TABLE0_1', sk=nompar)
20  continue
!
    call jedema()
!
end subroutine
