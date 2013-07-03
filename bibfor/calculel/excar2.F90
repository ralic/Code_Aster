subroutine excar2(ngrmx, desc, dg, ncmp, debugr)
    implicit none
#include "jeveux.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/trigd.h"
    integer :: ngrmx, desc, dg(*), ncmp
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
!
! BUT : RECOPIER DANS LE CHAMP LOCAL, LES CMPS DE LA CARTE CORRESPONDANT
!       AU DESCRIPTEUR-GRANDEUR DG
!
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    common /caii04/iachii,iachik,iachix
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
    integer :: ptma, ptms, ient, ima, deb2
    integer :: debgd, indval, debugr
    integer :: iachii, iachik, iachix, ialiel, iamaco, iamsco
    integer :: iel, illiel, ilmaco, ilmsco
    integer :: numail, jparal, iexi
    logical :: lparal
!     -- FONCTIONS FORMULES :
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
    numail(igr,iel) = zi(ialiel-1+zi(illiel-1+igr)-1+iel)
!
! DEB-------------------------------------------------------------------
!
!     PARALLELE OR NOT ?
!     -------------------------
    call jeexin('&CALCUL.PARALLELE', iexi)
    if (iexi .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal=.false.
    endif
!
!
!     SI LA CARTE EST CONSTANTE:
!     --------------------------
    if (ngrmx .eq. 1 .and. zi(desc-1+4) .eq. 1) then
        debgd = 3 + 2*ngrmx + 1
        deb2 = debugr
        do 10,iel = 1,nbelgr
        if (lparal) then
            if (.not.zl(jparal-1+iel)) then
                deb2=deb2+ncmp
                goto 10
            endif
        endif
        call trigd(zi(desc-1+debgd), 1, dg, deb2, .false.,&
                   0, 0)
10      continue
        goto 40
    endif
!
!
!     SI LA CARTE N'EST PAS CONSTANTE :
!     ---------------------------------
    ptma = zi(iachii-1+11* (iichin-1)+6)
    ptms = zi(iachii-1+11* (iichin-1)+7)
!
    deb2 = debugr
    do 20,iel = 1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) then
            deb2=deb2+ncmp
            goto 20
        endif
    endif
!       RECHERCHE DU NUMERO DE L'ENTITE CORRESPONDANT A LA MAILLE IMA:
    ima = numail(igr,iel)
    if (ima .lt. 0) then
        ient = zi(ptms-1-ima)
    else
        ient = zi(ptma-1+ima)
    endif
    if (ient .eq. 0) then
        deb2=deb2+ncmp
        goto 20
    endif
!
!       RECOPIE:
!       -------
    debgd = 3 + 2*ngrmx + (ient-1)*nec + 1
    indval = (ient-1)*ncmpmx + 1
!
    call trigd(zi(desc-1+debgd), indval, dg, deb2, .false.,&
               0, 0)
    20 end do
!
40  continue
!
end subroutine
