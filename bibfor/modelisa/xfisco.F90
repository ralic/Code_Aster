subroutine xfisco(noma, modelx)
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
! person_in_charge: patrick.massin at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=8) :: noma, modelx
!
! CREATION D'UN CHAMP ÉLÉMENTAIRE DE CONNECTIVITÉ DES FISSURES BRANCHÉES
!
!
!
!
!
    integer :: jnbsp, jcesd, jcesv, jcesl, ibid, iret, nncp
    integer :: jcesd2, jcesv2, jcesl2, jjonf, jjonc, iad, iad3
    integer :: ima, nbma, ifiss, ifis2, ifis3, nfiss, nfis2
    character(len=19) :: ces, ces2, ligrel, chglo
    character(len=8) :: nomfis, nomfi3, licmp(2), kbid, valk(3)
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DU NOMBRE DE SOUS POINT (NBRE DE FISSURES VUES)
!
    call jeveuo('&&XTYELE.NBSP', 'L', jnbsp)
!
! --- CONSTRUCTION DU CHAMP SIMPLE TEMPORAIRE
!
    ces = '&&XFISCO.CES'
    licmp(1) = 'X1'
    licmp(2) = 'X2'
!
    call cescre('V', ces, 'ELEM', noma, 'NEUT_I',&
                2, licmp, [ibid], zi(jnbsp), [-2])
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESV', 'E', jcesv)
    call jeveuo(ces//'.CESL', 'E', jcesl)
!
! --- RECUPERATION DU CHAMP ELEM S CONTENANT LE NOM DES FISSURES VUES
!
    ces2 = '&&XCONNO.CES2'
    call jeveuo(ces2//'.CESD', 'L', jcesd2)
    call jeveuo(ces2//'.CESV', 'E', jcesv2)
    call jeveuo(ces2//'.CESL', 'E', jcesl2)
!
! --- RECUPERATION NOMBRE DE MAILLES
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                kbid, iret)
!
! --- BOUCLE SUR LES MAILLES
!
    do 100 ima = 1, nbma
        nfiss = zi(jnbsp-1+ima)
!
! --- BOUCLE SUR LES FISSURE DE LA MAILLE
!
        do 110 ifiss = 1, nfiss
            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                        ifiss, 1, iad)
            ASSERT(iad.gt.0)
            nomfis = zk8(jcesv2-1+iad)
            call jeexin(nomfis//'.JONFISS', iret)
            if (iret .ne. 0) then
                call jeveuo(nomfis//'.JONFISS', 'L', jjonf)
                call jeveuo(nomfis//'.JONCOEF', 'L', jjonc)
                call jelira(nomfis//'.JONFISS', 'LONMAX', nfis2)
!
! --- BOUCLE SUR LES FISSURES DE LA MAILLE IFIS3, AUTRE QUE IFISS
!
                do 130 ifis3 = 1, nfiss
                    if (ifis3 .eq. ifiss) goto 130
!
! --- RECUPERATION DU NOM GLOBALE  NOMFI3 DE IFIS3
!
                    call cesexi('S', jcesd2, jcesl2, ima, 1,&
                                ifis3, 1, iad)
                    nomfi3 = zk8(jcesv2-1+iad)
!
! --- ON REGARDE SI LA FISSURE NOMFI3 EST CONNECTÉ À NOMFIS
!
                    do 120 ifis2 = 1, nfis2
                        if (zk8(jjonf-1+ifis2) .eq. nomfi3) then
                            if (ifis3 .gt. ifiss) then
                                valk(1) = nomfis
                                valk(2) = nomfi3
                                call utmess('F', 'XFEM_46', nk=2, valk=valk)
                            endif
                            call cesexi('S', jcesd, jcesl, ima, 1,&
                                        ifiss, 1, iad)
                            if (iad .gt. 0) then
                                call cesexi('S', jcesd, jcesl, ima, 1,&
                                            ifis3, 1, iad3)
                                if (zi(jcesv-1+iad) .eq. zi(jcesv-1+ iad3)) then
                                    iad = -iad
                                else
                                    valk(1) = nomfis
                                    valk(3) = nomfi3
                                    call utmess('F', 'XFEM_47', nk=3, valk=valk)
                                endif
                            endif
                            valk(2) = nomfi3
                            zl(jcesl-1-iad) = .true.
                            zi(jcesv-1-iad) = ifis3
                            call cesexi('S', jcesd, jcesl, ima, 1,&
                                        ifiss, 2, iad)
                            if (iad .gt. 0) iad = -iad
                            zl(jcesl-1-iad) = .true.
                            zi(jcesv-1-iad) = zi(jjonc-1+ifis2)
                        endif
120                  continue
130              continue
            endif
!
! --- SI ON A RIEN TROUVER
!
            call cesexi('S', jcesd, jcesl, ima, 1,&
                        ifiss, 1, iad)
            if (iad .lt. 0) then
                zl(jcesl-1-iad) = .true.
                zi(jcesv-1-iad) = 0
                call cesexi('S', jcesd, jcesl, ima, 1,&
                            ifiss, 2, iad)
                ASSERT(iad.lt.0)
                zl(jcesl-1-iad) = .true.
                zi(jcesv-1-iad) = 0
            endif
110      continue
!
100  end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    chglo = modelx(1:8)//'.FISSCO'
    ligrel = modelx(1:8)//'.MODELE'
    call cescel(ces, ligrel, 'TOPOSE', 'PFISCO', 'OUI',&
                nncp, 'V', chglo, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces)
!
    call jedema()
end subroutine
