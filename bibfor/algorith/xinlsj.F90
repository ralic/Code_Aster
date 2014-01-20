subroutine xinlsj(noma, ndim, fiss, nfiss, cnslj)
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
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscre.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/padist.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: noma, fiss
    integer :: ndim, nfiss
    character(len=19) :: cnslj
!
! ----------------------------------------------------------------------
!
! CALCUL DU CHAMP LOCAL LEVEL-SET JONCTIONS
!
! ELLES SERVENT A DELIMITER LA ZONE D'ENRICHISSEMENT
!
!
!
!
    real(kind=8) :: point(3), dist, dmin
    integer :: jjonf, jjonc, jjon3,  ino, nbno, iret, ibid
    integer :: nfini, ifiss, nfis2, nfis3, ifis2, ifis3, cpt, jcnsvt, nfisd
    integer :: jcnsv, jcnsl, jcnsvn, coefln(10),  iadrco, nuno
    character(len=8) :: ch, nomfis(10)
    character(len=19) :: cnsln, cnslt, jonfis, joncoe
    character(len=8), pointer :: vfiss(:) => null()
    character(len=8), pointer :: licmp(:) => null()
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
    nfiss = -nfiss
    cnsln= '&&XINLSJ.CNSLN'
    cnslt= '&&XINLSJ.CNSLT'
    AS_ALLOCATE(vk8=vfiss, size=nfiss)
    call getvid('JONCTION', 'FISSURE', iocc=1, nbval=nfiss, vect=vfiss,&
                nbret=ibid)
    call getvr8('JONCTION', 'POINT', iocc=1, nbval=3, vect=point,&
                nbret=ibid)
!
! --- ACCES AU MAILLAGE
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', iadrco)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
! --- RECHERCHE DU NUMÉRO DU NOEUD NUNO LE PLUS PROCHE DU POINT
!
    dmin = r8maem()
    do ino = 1, nbno
        dist = padist(ndim,point,zr(iadrco+(ino-1)*3+1-1))
        if (dist .lt. dmin) then
            nuno = ino
            dmin = dist
        endif
    end do
!
! --- ON AJOUTE LES FISSURES DECLARÉES DANS LE MOT CLÉ JONCTION
    cpt = 0
    do ifiss = 1, nfiss
        do ifis2 = ifiss+1, nfiss
            call jeexin(vfiss(ifis2)//'.JONFISS', iret)
            if (iret .ne. 0) then
                call jeveuo(vfiss(ifis2)//'.JONFISS', 'L', jjon3)
                call jelira(vfiss(ifis2)//'.JONFISS', 'LONMAX', nfis3)
                do ifis3 = 1, nfis3
! --- SI IFISS EST CONTENU DANS LES FISSURES SUIVANTES : ON SORT
! --- ELLE SERA AJOUTÉ DANS LA BOUCLE 60
                    if (zk8(jjon3-1+ifis3) .eq. vfiss(ifiss)) goto 50
                end do
            endif
        end do
        cpt = cpt +1
        nomfis(cpt) = vfiss(ifiss)
        call cnocns(nomfis(cpt)//'.LNNO', 'V', cnsln)
        call jeveuo(cnsln//'.CNSV', 'L', jcnsvn)
        ASSERT(zr(jcnsvn-1+nuno).ne.0.d0)
        coefln(cpt) = nint(sign(1.d0,-1.d0*zr(jcnsvn-1+nuno)))
 50     continue
    end do
!
    nfini = 1
    nfiss = cpt
    nfisd = nfiss
! --- ON AJOUTE TOUTES LES FISSURES CONNECTÉES PRECEDEMENT
! --- SAUF CELLES QUI CONTIENNENT LA FISSURE FISS EN COURS
!
 90 continue
    do ifiss = nfini, nfiss
        call jeexin(nomfis(ifiss)//'.JONFISS', iret)
        if (iret .ne. 0) then
            call jeveuo(nomfis(ifiss)//'.JONFISS', 'L', jjonf)
            call jeveuo(nomfis(ifiss)//'.JONCOEF', 'L', jjonc)
            call jelira(nomfis(ifiss)//'.JONFISS', 'LONMAX', nfis2)
! --- BOUCLE SUR LES FISSURES CONNECTES À IFISS
            do ifis2 = 1, nfis2
! --- ON VERIFIE QUE LA FISSURE CONNECTEE NE CONTIENT PAS CELLE EN COURS
                call jeexin(zk8(jjonf-1+ifis2)//'.JONFISS', iret)
                if (iret .ne. 0) then
                    call jeveuo(zk8(jjonf-1+ifis2)//'.JONFISS', 'L', jjon3)
                    call jelira(zk8(jjonf-1+ifis2)//'.JONFISS', 'LONMAX', nfis3)
                    do ifis3 = 1, nfis3
                        if (zk8(jjon3-1+ifis3) .eq. fiss) goto 70
                    end do
                endif
! --- ON VERIFIE QU'ON A PAS DEJA STOCKÉ LA FISSURE DANS LA LISTE
                do ifis3 = 1, cpt
                    if (zk8(jjonf-1+ifis2) .eq. nomfis(ifis3)) goto 70
                end do
! --- ON AJOUTE LES FISSURES CONNECTÉS À LA LISTE
                cpt = cpt+1
                nomfis(cpt) = zk8(jjonf-1+ifis2)
                coefln(cpt) = zi(jjonc-1+ifis2)
 70             continue
            end do
        endif
    end do
    nfini = nfiss+1
    nfiss = cpt
    if (nfini .le. nfiss) goto 90
    ASSERT(nfiss.le.10)
!
! --- CRÉATION DES SD GLOBALES JONFISS ET JONCOEF
!
    jonfis = fiss(1:8)//'.JONFISS'
    joncoe = fiss(1:8)//'.JONCOEF'
    call wkvect(jonfis, 'G V K8', nfiss, jjonf)
    call wkvect(joncoe, 'G V I', nfiss, jjonc)
    do ifiss = 1, nfiss
        zk8(jjonf-1+ifiss) = nomfis(ifiss)
        zi(jjonc-1+ifiss) = coefln(ifiss)
    end do
!
    AS_ALLOCATE(vk8=licmp, size=2*nfiss)
    do ifiss = 1, 2*nfiss
        call codent(ifiss, 'G', ch)
        licmp(ifiss) = 'X'//ch
    end do
!
! --- CRÉATION DE LA SD CNSLJ : LSJ(IFISS,1) = COEF*LSN(IFISS)
!                               LSJ(IFISS,2) = LST(IFISS)
!
    call cnscre(noma, 'N120_R', 2*nfiss, licmp, 'V',&
                cnslj)
    call jeveuo(cnslj//'.CNSV', 'E', jcnsv)
    call jeveuo(cnslj//'.CNSL', 'E', jcnsl)
    do ifiss = 1, nfiss
        call cnocns(nomfis(ifiss)//'.LNNO', 'V', cnsln)
        call jeveuo(cnsln//'.CNSV', 'L', jcnsvn)
        call cnocns(nomfis(ifiss)//'.LTNO', 'V', cnslt)
        call jeveuo(cnslt//'.CNSV', 'L', jcnsvt)
        do ino = 1, nbno
            zl(jcnsl-1+2*nfiss*(ino-1)+2*(ifiss-1)+1) = .true.
            zr(jcnsv-1+2*nfiss*(ino-1)+2*(ifiss-1)+1) = coefln(ifiss)* zr(jcnsvn-1+ino)
            zl(jcnsl-1+2*nfiss*(ino-1)+2*(ifiss-1)+2) = .true.
            if (ifiss .le. nfisd) then
                zr(jcnsv-1+2*nfiss*(ino-1)+2*(ifiss-1)+2) = -1
            else
! --- CRITERE SUR LA LST POUR LES FISS NON DECLAREES PAR L'UTILISATEUR
                zr(jcnsv-1+2*nfiss*(ino-1)+2*(ifiss-1)+2) = zr(jcnsvt- 1+ino)
            endif
        end do
    end do
!
    AS_DEALLOCATE(vk8=vfiss)
    call jedetr('&&XINLSJ.COEF')
    AS_DEALLOCATE(vk8=licmp)
    call detrsd('CHAM_NO_S', cnsln)
    call detrsd('CHAM_NO_S', cnslt)
!
    call jedema()
end subroutine
