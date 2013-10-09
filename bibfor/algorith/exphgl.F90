subroutine exphgl(nomres, typsd, modcyc, profno, indirf,&
                  mailsk, nbsec, numdia, nbmode)
!-----------------------------------------------------------------------
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
    implicit none
!
!  BUT:
!
!  RESTITUER LES RESULTATS ISSUS D'UN CALCUL CYCLIQUE
!     => RESULTAT COMPOSE DEJA ALLOUE PAR LA
!        ROUTINE APPELLANTE
!
!  DONNEES DU PROFCHNO DEJA CONSTITUE ET DE LA TABLE INDIRECTION
!  DES NUMEROS EQUATIONS CORRESPONDANTES (COLLECTION NUMEROTEE
!  POINTEE PAR LES NUMEROS DE SECTEUR)
!-----------------------------------------------------------------------
!
! NOMRES  /I/: NOM UT DU CONCEPT RESULTAT A REMPLIR
! MODCYC  /I/: NOM UT DU RESULTAT ISSU DU CALCUL CYCLIQUE
! PROFNO  /I/: NOM K19 DU PROFIL CHAMNO DEJA CONSTITUE
! INDIRF  /I/: NOM K24 DE LA FAMILLE DES INDIRECTIONS
! MAILSK  /I/: NOM K8 DU MAILLAGE SKELETTE
! TYPSD   /I/: NOM DU TYPE DE STRUCTURE DE DONNEES RESULTAT
! NBSEC   /I/: NBRE DE SECTEUR
! NUMDIA  /I/: NUMERO DU DIAMETRE
!
!
!
!
!
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rotchm.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslipa.h"
#include "asterfort/rsnoch.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, modcyc, mailsk, k8b, modcys
    character(len=16) :: depl, typsd
    character(len=19) :: chamva, profno, chamno
    character(len=24) :: indirf, crefe(2), nomchc, pfchno, nomchs
    real(kind=8) :: depi, genek, beta
    integer :: nbmode, ibid, iret, neqsec, lttsc, llfreq, ltveco, ldfreq, ldkge
    integer :: ldmge, ldom2, ldomo, nbnot, nbcmp, llcham, nbsec, neq, ires2
    integer :: numdia, ltvesi
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, icomp, ieqf, ieqi, ier, j, k
    integer :: ldtyd, llinsk, ltinds, n1, nddcou
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    depi = r8depi()
!
!-----REMPLISSAGE DU CREFE POUR CREATION CHAMNO-------------------------
!
    crefe(1) = mailsk
    crefe(2) = profno
!
!-----RECUPERATION DU NOMBRE DE DDL PHYSIQUES DU SECTEUR----------------
!
    call rsexch('F', modcyc, 'DEPL', 1, chamno,&
                ier)
    call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=pfchno)
!
    call dismoi('NB_EQUA', pfchno, 'PROF_CHNO', repi=neqsec)
!     -- QUESTION "POURRIE" :
    call dismoi('NOM_GD ', pfchno, 'PROF_CHNO', repi=ibid, repk=k8b)
    call dismoi('NB_CMP_MAX', k8b, 'GRANDEUR', repi=nbcmp)
!
!-----RECUPERATION DU NOMBRE DE DDL PHYSIQUES GLOBAUX-------------------
!
    call jelira(profno//'.DEEQ', 'LONMAX', neq)
    neq = neq / 2
!
!-----RECUPERATION DES FREQUENCES---------------------------------------
!
    if ((typsd(1:9).eq.'MODE_MECA') .or. (typsd(1:4).eq.'BASE')) then
        call rslipa(modcyc, 'FREQ', '&&EXPHGL.LIR8', llfreq, n1)
    else
        call rslipa(modcyc, 'INST', '&&EXPHGL.LIR8', llfreq, n1)
    endif
!
!-----ALLOCATION DES VECTEURS DE TRAVAIL--------------------------------
!
    call wkvect('&&EXPHGL.VEC.REEL', 'V V R', neqsec, ltveco)
!
!-----CALCUL DU TETA DE CHAQUE SECTEUR----------------------------------
!
    call wkvect('&&EXPHGL.TETA_SECTEUR', 'V V R', nbsec, lttsc)
    do i = 1, nbsec
        zr(lttsc+i-1) = depi*(i-1) / nbsec
    end do
!
!-----RECUPERATION DE L'INDIRECTION SQUELETTE---------------------------
!
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
    call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
!
!***********************************************************************
!
    call getvid('CYCLIQUE', 'RESULTAT2', iocc=1, scal=modcys, nbret=ires2)
!
    icomp = 0
!
!  CALCUL DU DEPHASAGE INTER-SECTEUR
!
    beta = numdia*(depi/nbsec)
!
!  BOUCLE SUR LES MODES PROPRES DU DIAMETRE COURANT
!
    do i = 1, nbmode
        icomp = icomp + 1
        call rsexch('F', modcyc, 'DEPL', i, nomchc,&
                    iret)
        call jeveuo(nomchc(1:19)//'.VALE', 'L', ltveco)
        if (ires2 .ne. 0) then
            call rsexch('F', modcys, 'DEPL', i, nomchs,&
                        iret)
            call jeveuo(nomchs(1:19)//'.VALE', 'L', ltvesi)
        endif
!
!
!***********************************************************************
!
        call rsexch(' ', nomres, depl, i, chamva,&
                    iret)
        call vtcrea(chamva, crefe, 'G', 'R', neq)
        call rsnoch(nomres, depl, i)
        call jeveuo(chamva//'.VALE', 'E', llcham)
!
!  COMMUN POUR MODE_MECA ET BASE_MODALE
!
        if ((typsd(1:9).eq.'MODE_MECA')) then
            call rsadpa(nomres, 'E', 1, 'FREQ', i,&
                        0, sjv=ldfreq, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'RIGI_GENE', i,&
                        0, sjv=ldkge, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'MASS_GENE', i,&
                        0, sjv=ldmge, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'OMEGA2', i,&
                        0, sjv=ldom2, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'NUME_MODE', i,&
                        0, sjv=ldomo, styp=k8b)
            genek = (zr(llfreq+icomp-1)*depi)**2
            zr(ldfreq) = zr(llfreq+icomp-1)
            zr(ldkge) = genek
            zr(ldmge) = 1.d0
            zr(ldom2) = genek
            zi(ldomo) = i
!
!  SPECIFIQUE A BASE_MODALE
!
            call rsadpa(nomres, 'E', 1, 'TYPE_DEFO', i,&
                        0, sjv=ldtyd, styp=k8b)
            zk16(ldtyd) = 'PROPRE          '
        else
            call rsadpa(nomres, 'E', 1, 'INST', i,&
                        0, sjv=ldfreq, styp=k8b)
            zr(ldfreq) = zr(llfreq+icomp-1)
        endif
!
!  BOUCLE SUR LES SECTEURS
!
        do k = 1, nbsec
            call jeveuo(jexnum(indirf, k), 'L', ltinds)
            call jelira(jexnum(indirf, k), 'LONMAX', nddcou)
            nddcou = nddcou/2
            do j = 1, nddcou
                ieqi = zi(ltinds+(j-1)*2)
                ieqf = zi(ltinds+(j-1)*2+1)
                if (ires2 .ne. 0) then
                    zr(llcham+ieqf-1) = sin(&
                                        (k-1)*beta)*zr(ltveco+ ieqi-1) +cos((k-1)*beta)*zr(ltvesi&
                                        &+ieqi-1&
                                        )
                else
                    zr(llcham+ieqf-1) = zr(ltveco+ieqi-1)
                endif
            end do
        end do
!
!  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
!
        call rotchm(profno, zr(llcham), zr(lttsc), nbsec, zi(llinsk),&
                    nbnot, nbcmp, 3)
!
        call jelibe(nomchc(1:19)//'.VALE')
        if (ires2 .ne. 0) then
            call jelibe(nomchs(1:19)//'.VALE')
        endif
    end do
!
    call jedetr('&&EXPHGL.VEC.REEL')
    call jedetr('&&EXPHGL.ORDRE.FREQ')
    call jedetr('&&EXPHGL.TETA_SECTEUR')
    call jedetr('&&EXPHGL.TETGD')
    call jedetr('&&EXPHGL.LIR8')
!
    call jedema()
end subroutine
