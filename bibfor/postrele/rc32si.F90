subroutine rc32si()
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     RECUPERATION DES DONNEES DE "SITUATION"
!
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/codent.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ordis.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!
    integer :: n1, nbsitu, iocc, nume, ii, nocc, ing, jnbocc, jnumgr
    integer :: jpresa, jpresb, nbchar, jchar, jnsitu, jcombi,  ig
    integer :: numpas(2), nbvg, jnbvg, nbgr, numgr, nbsigr, jnsg, nbth, jseigr
    integer :: nscy, nbm, vali(3), nbgrt, numg1, numg2, nbseis, numgs, nbsg1
    integer :: nbsg2, nbsg3, nbp12, nbp23, nbp13, ndim, jspas, jsigr, jsp12
    integer :: jsp23, jsp13
    logical :: yapass
    character(len=8) :: k8b, knume, ouinon
    character(len=16) :: motcl1, motcl2
    integer, pointer :: nume_group(:) => null()
! DEB ------------------------------------------------------------------
!
    motcl1 = 'SITUATION'
    motcl2 = 'SEISME'
!
    call getfac(motcl1, nbsitu)
    call getfac(motcl2, nbseis)
!
    ndim = nbsitu + nbseis
    AS_ALLOCATE(vi=nume_group, size=ndim)
    call wkvect('&&RC32SI.SITU_GROUP', 'V V I', 2*ndim, jsigr)
!
    call wkvect('&&RC3200.SITU_NUMERO', 'V V I', ndim, jnsitu)
    call wkvect('&&RC3200.SITU_NB_OCCUR', 'V V I', 2*ndim, jnbocc)
    call wkvect('&&RC3200.SITU_PRES_A', 'V V R', nbsitu, jpresa)
    call wkvect('&&RC3200.SITU_PRES_B', 'V V R', nbsitu, jpresb)
    call wkvect('&&RC3200.SITU_COMBINABLE', 'V V L', ndim, jcombi)
    call jecrec('&&RC3200.SITU_ETAT_A', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                ndim)
    call jecrec('&&RC3200.SITU_ETAT_B', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsitu)
    call jecrec('&&RC3200.SITU_THERMIQUE', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                ndim)
!
    call wkvect('&&RC32SI.PASSAGE_1_2', 'V V I', ndim, jsp12)
    call wkvect('&&RC32SI.PASSAGE_2_3', 'V V I', ndim, jsp23)
    call wkvect('&&RC32SI.PASSAGE_1_3', 'V V I', ndim, jsp13)
    call jeecra('&&RC32SI.PASSAGE_1_2', 'LONUTI', 0)
    call jeecra('&&RC32SI.PASSAGE_2_3', 'LONUTI', 0)
    call jeecra('&&RC32SI.PASSAGE_1_3', 'LONUTI', 0)
!
    nbgr = 0
    yapass = .false.
!
    do 10, iocc = 1, nbsitu, 1
!
    call codent(iocc, 'D0', k8b)
!
! ------ LE NUMERO DE SITUATION:
!        -----------------------
!
    call getvis(motcl1, 'NUME_SITU', iocc=iocc, scal=nume, nbret=n1)
    zi(jnsitu+iocc-1) = nume
    knume = 'S       '
    call codent(nume, 'D0', knume(2:8))
!
! ------ LE NOMBRE D'OCCURRENCE:
!        -----------------------
!
    call getvis(motcl1, 'NB_OCCUR', iocc=iocc, scal=nocc, nbret=n1)
    zi(jnbocc+2*iocc-2) = nocc
!
! ------ LES PRESSIONS:
!        --------------
!
    call getvr8(motcl1, 'PRES_A', iocc=iocc, scal=zr(jpresa+iocc-1), nbret=n1)
    call getvr8(motcl1, 'PRES_B', iocc=iocc, scal=zr(jpresb+iocc-1), nbret=n1)
!
! ------ LES NUMEROS DE GROUPE:
!        ----------------------
!
    call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        nbvg = -n1
        call wkvect('&&RC32SI.VALE_GR', 'V V I', nbvg, jnbvg)
        call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi( jnbvg),&
                    nbret=n1)
        do 26 ing = 1, nbvg
            numgr = zi(jnbvg+ing-1)
            if (numgr .le. 0) then
                call utmess('F', 'POSTRCCM_12')
            endif
            do 20 ig = 1, nbgr
                if (nume_group(ig) .eq. numgr) goto 21
20          continue
            nbgr = nbgr + 1
            nume_group(nbgr) = numgr
21          continue
26      continue
        if (nbvg .eq. 1) then
            zi(jsigr+2*iocc-2) = zi(jnbvg)
            zi(jsigr+2*iocc-1) = zi(jnbvg)
        else
            zi(jsigr+2*iocc-2) = zi(jnbvg)
            zi(jsigr+2*iocc-1) = zi(jnbvg+1)
        endif
        call jedetr('&&RC32SI.VALE_GR')
    endif
!
! ------ LES NUMEROS DE PASSAGE:
!        -----------------------
!
    call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
                    nbret=n1)
        if (numpas(1) .le. 0) then
            call utmess('F', 'POSTRCCM_12')
        endif
        if (numpas(2) .le. 0) then
            call utmess('F', 'POSTRCCM_12')
        endif
        if (numpas(1) .gt. 3) then
            call utmess('F', 'POSTRCCM_12')
        endif
        if (numpas(2) .gt. 3) then
            call utmess('F', 'POSTRCCM_12')
        endif
        yapass = .true.
        zi(jsigr+2*iocc-2) = min ( numpas(1), numpas(2) )
        zi(jsigr+2*iocc-1) = max ( numpas(1), numpas(2) )
        numgr = numpas(1)
        do 22 ig = 1, nbgr
            if (nume_group(ig) .eq. numgr) goto 23
22      continue
        nbgr = nbgr + 1
        nume_group(nbgr) = numgr
23      continue
        numgr = numpas(2)
        do 24 ig = 1, nbgr
            if (nume_group(ig) .eq. numgr) goto 25
24      continue
        nbgr = nbgr + 1
        nume_group(nbgr) = numgr
25      continue
    endif
!
! ------ COMBINABLE DANS SON GROUPE:
!        ---------------------------
    call getvtx(motcl1, 'COMBINABLE', iocc=iocc, scal=ouinon, nbret=n1)
    if (ouinon(1:3) .eq. 'OUI') then
        zl(jcombi+iocc-1) = .true.
    else
        zl(jcombi+iocc-1) = .false.
    endif
!
! ------ ETAT DE CHARGEMENT POUR "A":
!        ----------------------------
    call getvis(motcl1, 'CHAR_ETAT_A', iocc=iocc, nbval=0, nbret=n1)
    nbchar = -n1
    call jecroc(jexnom('&&RC3200.SITU_ETAT_A', knume))
    call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONMAX', nbchar)
    call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONUTI', nbchar)
    call jeveuo(jexnom('&&RC3200.SITU_ETAT_A', knume), 'E', jchar)
    call getvis(motcl1, 'CHAR_ETAT_A', iocc=iocc, nbval=nbchar, vect=zi(jchar),&
                nbret=n1)
!
! ------ ETAT DE CHARGEMENT POUR "B":
!        ----------------------------
    call getvis(motcl1, 'CHAR_ETAT_B', iocc=iocc, nbval=0, nbret=n1)
    nbchar = -n1
    call jecroc(jexnom('&&RC3200.SITU_ETAT_B', knume))
    call jeecra(jexnom('&&RC3200.SITU_ETAT_B', knume), 'LONMAX', nbchar)
    call jeecra(jexnom('&&RC3200.SITU_ETAT_B', knume), 'LONUTI', nbchar)
    call jeveuo(jexnom('&&RC3200.SITU_ETAT_B', knume), 'E', jchar)
    call getvis(motcl1, 'CHAR_ETAT_B', iocc=iocc, nbval=nbchar, vect=zi(jchar),&
                nbret=n1)
!
! ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
!        ---------------------------------------------
    call getvis(motcl1, 'NUME_RESU_THER', iocc=iocc, nbval=0, nbret=n1)
    nbth = -n1
    call jecroc(jexnom('&&RC3200.SITU_THERMIQUE', knume))
    nbm = max(1,nbth)
    call jeecra(jexnom('&&RC3200.SITU_THERMIQUE', knume), 'LONMAX', nbm)
!
    if (nbth .eq. 0) then
        call jeecra(jexnom('&&RC3200.SITU_THERMIQUE', knume), 'LONUTI', 0)
    else
        call jeecra(jexnom('&&RC3200.SITU_THERMIQUE', knume), 'LONUTI', nbth)
        call jeveuo(jexnom('&&RC3200.SITU_THERMIQUE', knume), 'E', jchar)
        call getvis(motcl1, 'NUME_RESU_THER', iocc=iocc, nbval=nbth, vect=zi(jchar),&
                    nbret=n1)
    endif
!
    10 end do
!
    do 110, iocc = 1, nbseis, 1
!
    call getvis(motcl2, 'NUME_GROUPE', iocc=iocc, scal=nume, nbret=n1)
    zi(jsigr+2*(nbsitu+iocc)-2) = nume
    zi(jsigr+2*(nbsitu+iocc)-1) = nume
!
    zl(jcombi+nbsitu+iocc-1) = .true.
!
! ------ LE NUMERO DE SITUATION:
!        -----------------------
    call getvis(motcl2, 'NUME_SITU', iocc=iocc, scal=nume, nbret=n1)
    zi(jnsitu+nbsitu+iocc-1) = nume
    knume = 'S       '
    call codent(nume, 'D0', knume(2:8))
!
! ------ LE NOMBRE D'OCCURRENCE DE SEISME:
!        ---------------------------------
    call getvis(motcl2, 'NB_OCCUR', iocc=iocc, scal=nocc, nbret=n1)
    zi(jnbocc+2*(nbsitu+iocc)-2) = nocc
    call getvis(motcl2, 'NB_CYCL_SEISME', iocc=iocc, scal=nscy, nbret=n1)
    zi(jnbocc+2*(nbsitu+iocc)-1) = nscy
!
! ------ ETAT DE CHARGEMENT:
!        -------------------
    call getvis(motcl2, 'CHAR_ETAT', iocc=iocc, nbval=0, nbret=n1)
    nbchar = -n1
    call jecroc(jexnom('&&RC3200.SITU_ETAT_A', knume))
    call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONMAX', nbchar)
    call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONUTI', nbchar)
    call jeveuo(jexnom('&&RC3200.SITU_ETAT_A', knume), 'E', jchar)
    call getvis(motcl2, 'CHAR_ETAT', iocc=iocc, nbval=nbchar, vect=zi(jchar),&
                nbret=n1)
!
! ------ SEISME : PAS DE TRANSITOIRE THERMIQUE
!        ---------------------------------------------
    nbth = 0
    call jecroc(jexnom('&&RC3200.SITU_THERMIQUE', knume))
    nbm = max(1,nbth)
    call jeecra(jexnom('&&RC3200.SITU_THERMIQUE', knume), 'LONMAX', nbm)
    call jeecra(jexnom('&&RC3200.SITU_THERMIQUE', knume), 'LONUTI', 0)
!
    110 end do
!
    call ordis(nume_group, nbgr)
!
    if (nbgr .gt. 3 .and. yapass) then
        call utmess('F', 'POSTRCCM_34')
    endif
!
!     ------------------------------------------------------------------
! --- ON AJOUTE 1 GROUPE POUR LES SITUATIONS DE PASSAGE
    if (yapass) nbgr = nbgr + 1
!
!     ------------------------------------------------------------------
! --- DEFINITION DES GROUPES
    call wkvect('&&RC3200.SITU_NUME_GROUP', 'V V I', nbgr, jnumgr)
    call wkvect('&&RC3200.SITU_SEISME', 'V V I', nbgr, jseigr)
    call jecrec('&&RC3200.LES_GROUPES', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbgr)
!
    if (yapass) then
        nbgrt = nbgr - 1
    else
        nbgrt = nbgr
    endif
    do 30 ig = 1, nbgrt, 1
!
        numgr = nume_group(ig)
!
        zi(jnumgr+ig-1) = numgr
!
! ------ ON COMPTE LES SITUATIONS DU GROUPE
        nbsigr = 0
        do 32, iocc = 1, nbsitu, 1
        call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbvg = -n1
            call wkvect('&&RC32SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi(jnbvg),&
                        nbret=n1)
            do 321 ing = 1, nbvg
                if (zi(jnbvg+ing-1) .eq. numgr) nbsigr = nbsigr + 1
321          continue
            call jedetr('&&RC32SI.VALE_GR')
        endif
        call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
                        nbret=n1)
            if (numpas(1) .eq. numgr) nbsigr = nbsigr + 1
            if (numpas(2) .eq. numgr) nbsigr = nbsigr + 1
        endif
32      continue
!
! ------ ON COMPTE LES SITUATIONS DE SEISME
!
        do 36, iocc = 1, nbseis, 1
        call getvis(motcl2, 'NUME_GROUPE', iocc=iocc, scal=numgs, nbret=n1)
        if (numgs .eq. numgr) nbsigr = nbsigr + 1
36      continue
!
! ------ ON STOCKE LE NUMERO DE L'OCCURRENCE
        call jecroc(jexnum('&&RC3200.LES_GROUPES', numgr))
        call jeecra(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONMAX', nbsigr)
        call jeecra(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
        call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'E', jnsg)
        ii = 0
        do 34, iocc = 1, nbsitu, 1
        call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbvg = -n1
            call wkvect('&&RC32SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi(jnbvg),&
                        nbret=n1)
            do 341 ing = 1, nbvg
                if (zi(jnbvg+ing-1) .eq. numgr) then
                    ii = ii + 1
                    zi(jnsg+ii-1) = iocc
                endif
341          continue
            call jedetr('&&RC32SI.VALE_GR')
        endif
        call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
                        nbret=n1)
            if (numpas(1) .eq. numgr) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
            if (numpas(2) .eq. numgr) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
        endif
34      continue
        do 38, iocc = 1, nbseis, 1
        call getvis(motcl2, 'NUME_GROUPE', iocc=iocc, scal=numgs, nbret=n1)
        if (numgs .eq. numgr) then
            ii = ii + 1
            zi(jnsg+ii-1) = nbsitu+iocc
            if (zi(jseigr+ig-1) .ne. 0) then
                vali(1) = numgr
                vali(2) = iocc
                vali(3) = zi(jseigr+ig-1)
                call utmess('F', 'POSTRCCM_26', ni=3, vali=vali)
            endif
            zi(jseigr+ig-1) = iocc
        endif
38      continue
!
30  end do
!     ------------------------------------------------------------------
! --- TRAITEMENT DES SITUATIONS DE PASSAGE
    if (yapass) then
!
        call wkvect('&&RC32SI.PASSAGE_SIT', 'V V I', 3, jspas)
!
        nbsg1 = 0
        nbsg2 = 0
        nbsg3 = 0
        nbp12 = 0
        nbp23 = 0
        nbp13 = 0
        do 40, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 1 .and. numg2 .eq. 1) then
            nbsg1 = nbsg1 + 1
        else if (numg1.eq.1 .and. numg2.eq.2) then
            nbsg1 = nbsg1 + 1
            nbp12 = nbp12 + 1
            zi(jsp12+nbp12-1) = iocc
        else if (numg1.eq.2 .and. numg2.eq.2) then
            nbsg2 = nbsg2 + 1
        else if (numg1.eq.2 .and. numg2.eq.3) then
            nbsg2 = nbsg2 + 1
            nbp23 = nbp23 + 1
            zi(jsp23+nbp23-1) = iocc
        else if (numg1.eq.3 .and. numg2.eq.3) then
            nbsg3 = nbsg3 + 1
        else if (numg1.eq.1 .and. numg2.eq.3) then
            nbsg3 = nbsg3 + 1
            nbp13 = nbp13 + 1
            zi(jsp13+nbp13-1) = iocc
        endif
40      continue
        call jeecra('&&RC32SI.PASSAGE_1_2', 'LONUTI', nbp12)
        call jeecra('&&RC32SI.PASSAGE_2_3', 'LONUTI', nbp23)
        call jeecra('&&RC32SI.PASSAGE_1_3', 'LONUTI', nbp13)
        zi(jspas ) = nbsg1
        zi(jspas+1) = nbsg2
        zi(jspas+2) = nbsg3
!
        zi(jnumgr+nbgr-1) = -nbgr
        call jecroc(jexnum('&&RC3200.LES_GROUPES', nbgr))
        call jeecra(jexnum('&&RC3200.LES_GROUPES', nbgr), 'LONMAX', ndim)
        call jeveuo(jexnum('&&RC3200.LES_GROUPES', nbgr), 'E', jnsg)
!
        ii = 0
        do 42, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 1 .and. numg2 .eq. 1) then
            ii = ii + 1
            zi(jnsg+ii-1) = iocc
        endif
42      continue
        do 44, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 1 .and. numg2 .eq. 2) then
            ii = ii + 1
            zi(jnsg+ii-1) = iocc
        endif
44      continue
        do 46, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 2 .and. numg2 .eq. 2) then
            ii = ii + 1
            zi(jnsg+ii-1) = iocc
        endif
46      continue
        do 48, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 2 .and. numg2 .eq. 3) then
            ii = ii + 1
            zi(jnsg+ii-1) = iocc
        endif
48      continue
        do 50, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 3 .and. numg2 .eq. 3) then
            ii = ii + 1
            zi(jnsg+ii-1) = iocc
        endif
50      continue
        do 52, iocc = 1, ndim, 1
        numg1 = zi(jsigr+2*iocc-2)
        numg2 = zi(jsigr+2*iocc-1)
        if (numg1 .eq. 1 .and. numg2 .eq. 3) then
            ii = ii + 1
            zi(jnsg+ii-1) = iocc
        endif
52      continue
        call jeecra(jexnum('&&RC3200.LES_GROUPES', nbgr), 'LONUTI', ii)
    endif
!
    AS_DEALLOCATE(vi=nume_group)
!
end subroutine
