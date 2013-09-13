subroutine rc36si(noma, nbma, listma)
    implicit none
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
#include "asterfort/jexnum.h"
#include "asterfort/ordis.h"
#include "asterfort/rc36cm.h"
#include "asterfort/rc36th.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbma, listma(*)
    character(len=8) :: noma
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     RECUPERATION DES DONNEES DE "SITUATION"
!
! IN  : NOMA   : MAILLAGE
! IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
! IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
!     ------------------------------------------------------------------
!
    integer :: n1, nbsitu, iocc, jmomea, jmomeb, ii, nocc, jreth, jnbocc
    integer :: jnumgr, jpresa, jpresb, nbchar, jchar1, jchar2, jnsitu, jcombi
    integer :: jpassa, jnbgr, ig, numpas(2), nscy, nbgr, numgr, nbsigr, jnsg
    integer :: nbth, jseigr, jchth, nume, nbm, nbp12, nbp23, nbp13, jsp12, jsp23
    integer :: jsp13, nbsg1, nbsg2, nbsg3, jsigr, vali(3), nbgrt, numg1, numg2
    integer :: jspas, ing, jnbvg, nbvg, ndim, numgs, nbseis
    logical :: yapass
    character(len=8) :: k8b, ouinon
    character(len=16) :: motcl1, motcl2
    character(len=24) :: chmome
! DEB ------------------------------------------------------------------
!
    motcl1 = 'SITUATION'
    motcl2 = 'SEISME'
!
    call getfac(motcl1, nbsitu)
    call getfac(motcl2, nbseis)
!
    ndim = nbsitu + nbseis
    call wkvect('&&RC36SI.NUME_GROUP', 'V V I', ndim, jnbgr)
    call wkvect('&&RC32SI.SITU_GROUP', 'V V I', 2*ndim, jsigr)
!
    call wkvect('&&RC3600.SITU_NUMERO', 'V V I', ndim, jnsitu)
    call wkvect('&&RC3600.SITU_NB_OCCUR', 'V V I', 2*ndim, jnbocc)
    call wkvect('&&RC3600.SITU_PRES_A', 'V V R', nbsitu, jpresa)
    call wkvect('&&RC3600.SITU_PRES_B', 'V V R', nbsitu, jpresb)
    call wkvect('&&RC3600.SITU_COMBINABLE', 'V V L', ndim, jcombi)
    call wkvect('&&RC3600.SITU_PASSAGE', 'V V I', 2*nbsitu, jpassa)
    call wkvect('&&RC3600.SITU_MOMENT_A', 'V V K24', ndim, jmomea)
    call wkvect('&&RC3600.SITU_MOMENT_B', 'V V K24', nbsitu, jmomeb)
    call jecrec('&&RC3600.SITU_THERMIQUE', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                ndim)
    call wkvect('&&RC3600.CHAM_THER', 'V V K24', ndim, jchth)
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
    call getvis(motcl1, 'NUME_SITU', iocc=iocc, scal=zi(jnsitu+iocc-1), nbret=n1)
!
! ------ LE NOMBRE D'OCCURRENCE:
!        -----------------------
    call getvis(motcl1, 'NB_OCCUR', iocc=iocc, scal=nocc, nbret=n1)
    zi(jnbocc+2*iocc-2) = nocc
!
! ------ LES PRESSIONS:
!        --------------
    call getvr8(motcl1, 'PRES_A', iocc=iocc, scal=zr(jpresa+iocc-1), nbret=n1)
    call getvr8(motcl1, 'PRES_B', iocc=iocc, scal=zr(jpresb+iocc-1), nbret=n1)
!
! ------ LES NUMEROS DE GROUPE:
!        ----------------------
    call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        nbvg = -n1
        call wkvect('&&RC36SI.VALE_GR', 'V V I', nbvg, jnbvg)
        call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi( jnbvg),&
                    nbret=n1)
        do 26 ing = 1, nbvg
            numgr = zi(jnbvg+ing-1)
            if (numgr .le. 0) call u2mess('F', 'POSTRCCM_12')
            do 20 ig = 1, nbgr
                if (zi(jnbgr+ig-1) .eq. numgr) goto 21
20          continue
            nbgr = nbgr + 1
            zi(jnbgr+nbgr-1) = numgr
21          continue
26      continue
        if (nbvg .eq. 1) then
            zi(jsigr+2*iocc-2) = zi(jnbvg)
            zi(jsigr+2*iocc-1) = zi(jnbvg)
        else
            zi(jsigr+2*iocc-2) = zi(jnbvg)
            zi(jsigr+2*iocc-1) = zi(jnbvg+1)
        endif
        call jedetr('&&RC36SI.VALE_GR')
    endif
!
! ------ LES NUMEROS DE PASSAGE:
!        -----------------------
    call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvis(motcl1, 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
                    nbret=n1)
        if (numpas(1) .le. 0) call u2mess('F', 'POSTRCCM_12')
        if (numpas(2) .le. 0) call u2mess('F', 'POSTRCCM_12')
        if (numpas(1) .gt. 3) call u2mess('F', 'POSTRCCM_12')
        if (numpas(2) .gt. 3) call u2mess('F', 'POSTRCCM_12')
        yapass = .true.
        zi(jsigr+2*iocc-2) = min ( numpas(1), numpas(2) )
        zi(jsigr+2*iocc-1) = max ( numpas(1), numpas(2) )
        numgr = numpas(1)
        do 22 ig = 1, nbgr
            if (zi(jnbgr+ig-1) .eq. numgr) goto 23
22      continue
        nbgr = nbgr + 1
        zi(jnbgr+nbgr-1) = numgr
23      continue
        numgr = numpas(2)
        do 24 ig = 1, nbgr
            if (zi(jnbgr+ig-1) .eq. numgr) goto 25
24      continue
        nbgr = nbgr + 1
        zi(jnbgr+nbgr-1) = numgr
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
    call wkvect('&&RC36SI.CHAR_ETAT', 'V V I', nbchar, jchar1)
    call getvis(motcl1, 'CHAR_ETAT_A', iocc=iocc, nbval=nbchar, vect=zi(jchar1),&
                nbret=n1)
!
    chmome = '&&RC36SI_A'//k8b
    call rc36cm(iocc, 'A', nbma, listma, nbchar,&
                zi(jchar1), chmome)
    zk24(jmomea+iocc-1) = chmome
    call jedetr('&&RC36SI.CHAR_ETAT')
!
! ------ ETAT DE CHARGEMENT POUR "B":
!        ----------------------------
    call getvis(motcl1, 'CHAR_ETAT_B', iocc=iocc, nbval=0, nbret=n1)
    nbchar = -n1
    call wkvect('&&RC36SI.CHAR_MECA', 'V V I', nbchar, jchar2)
    call getvis(motcl1, 'CHAR_ETAT_B', iocc=iocc, nbval=nbchar, vect=zi(jchar2),&
                nbret=n1)
!
    chmome = '&&RC36SI_B'//k8b
    call rc36cm(iocc, 'B', nbma, listma, nbchar,&
                zi(jchar2), chmome)
    zk24(jmomeb+iocc-1) = chmome
    call jedetr('&&RC36SI.CHAR_MECA')
!
! ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
!        ---------------------------------------------
    call getvis(motcl1, 'NUME_RESU_THER', iocc=iocc, nbval=0, nbret=n1)
    nbth = -n1
    call jecroc(jexnum('&&RC3600.SITU_THERMIQUE', iocc))
    nbm = max(1,nbth)
    call jeecra(jexnum('&&RC3600.SITU_THERMIQUE', iocc), 'LONMAX', nbm)
!
    if (nbth .eq. 0) then
        call jeecra(jexnum('&&RC3600.SITU_THERMIQUE', iocc), 'LONUTI', 0)
    else
        call jeecra(jexnum('&&RC3600.SITU_THERMIQUE', iocc), 'LONUTI', nbth)
        call jeveuo(jexnum('&&RC3600.SITU_THERMIQUE', iocc), 'E', jreth)
        call getvis(motcl1, 'NUME_RESU_THER', iocc=iocc, nbval=nbth, vect=zi(jreth),&
                    nbret=n1)
!     ------------------------------------------------------------------
!                   RESULTATS DES CALCULS THERMIQUES
!     ------------------------------------------------------------------
        call rc36th(noma, nbma, listma, zk24(jchth), iocc,&
                    nbth, zi( jreth))
    endif
!
    10 end do
!
    do 110, iocc = 1, nbseis, 1
!
    call codent(nbsitu+iocc, 'D0', k8b)
!
    call getvis(motcl2, 'NUME_GROUPE', iocc=iocc, scal=nume, nbret=n1)
    zi(jsigr+2*(nbsitu+iocc)-2) = nume
    zi(jsigr+2*(nbsitu+iocc)-1) = nume
!
    zl(jcombi+nbsitu+iocc-1) = .true.
!
! ------ LE NUMERO DE SITUATION:
!        -----------------------
    call getvis(motcl2, 'NUME_SITU', iocc=iocc, scal=zi(jnsitu+nbsitu+ iocc-1), nbret=n1)
!
! ------ LE NOMBRE D'OCCURRENCE:
!        -----------------------
    call getvis(motcl2, 'NB_OCCUR', iocc=iocc, scal=nocc, nbret=n1)
    zi(jnbocc+2*(nbsitu+iocc)-2) = nocc
    call getvis(motcl2, 'NB_CYCL_SEISME', iocc=iocc, scal=nscy, nbret=n1)
    zi(jnbocc+2*(nbsitu+iocc)-1) = nscy
!
! ------ ETAT DE CHARGEMENT:
!        -------------------
    call getvis(motcl2, 'CHAR_ETAT', iocc=iocc, nbval=0, nbret=n1)
    nbchar = -n1
    call wkvect('&&RC36SI.CHAR_ETAT', 'V V I', nbchar, jchar1)
    call getvis(motcl2, 'CHAR_ETAT', iocc=iocc, nbval=nbchar, vect=zi(jchar1),&
                nbret=n1)
!
    chmome = '&&RC36SI_A'//k8b
    call rc36cm(iocc, 'S', nbma, listma, nbchar,&
                zi(jchar1), chmome)
    zk24(jmomea+nbsitu+iocc-1) = chmome
    call jedetr('&&RC36SI.CHAR_ETAT')
!
! ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
!        ---------------------------------------------
    nbth = 0
    call jecroc(jexnum('&&RC3600.SITU_THERMIQUE', nbsitu+iocc))
    nbm = max(1,nbth)
    call jeecra(jexnum('&&RC3600.SITU_THERMIQUE', nbsitu+iocc), 'LONMAX', nbm)
!
    call jeecra(jexnum('&&RC3600.SITU_THERMIQUE', nbsitu+iocc), 'LONUTI', 0)
!
    110 end do
!
    call ordis(zi(jnbgr), nbgr)
!
    if (nbgr .gt. 3 .and. yapass) call u2mess('F', 'POSTRCCM_34')
!
!     ------------------------------------------------------------------
! --- ON AJOUTE 1 GROUPE POUR LES SITUATIONS DE PASSAGE
    if (yapass) nbgr = nbgr + 1
!
!     ------------------------------------------------------------------
! --- DEFINITION DES GROUPES
    call wkvect('&&RC3600.SITU_NUME_GROUP', 'V V I', nbgr, jnumgr)
    call wkvect('&&RC3600.SITU_SEISME', 'V V I', nbgr, jseigr)
    call jecrec('&&RC3600.LES_GROUPES', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbgr)
!
    if (yapass) then
        nbgrt = nbgr - 1
    else
        nbgrt = nbgr
    endif
    do 30 ig = 1, nbgrt, 1
!
        numgr = zi(jnbgr+ig-1)
!
        zi(jnumgr+ig-1) = numgr
!
! ------ ON COMPTE LES SITUATIONS DU GROUPE
        nbsigr = 0
        do 32, iocc = 1, nbsitu, 1
        call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbvg = -n1
            call wkvect('&&RC36SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi(jnbvg),&
                        nbret=n1)
            do 321 ing = 1, nbvg
                if (zi(jnbvg+ing-1) .eq. numgr) nbsigr = nbsigr + 1
321          continue
            call jedetr('&&RC36SI.VALE_GR')
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
        do 36, iocc = 1, nbseis, 1
        call getvis(motcl2, 'NUME_GROUPE', iocc=iocc, scal=numgs, nbret=n1)
        if (numgs .eq. numgr) nbsigr = nbsigr + 1
36      continue
!
! ------ ON STOCKE LE NUMERO DE L'OCCURRENCE
        call jecroc(jexnum('&&RC3600.LES_GROUPES', numgr))
        call jeecra(jexnum('&&RC3600.LES_GROUPES', numgr), 'LONMAX', nbsigr)
        call jeveuo(jexnum('&&RC3600.LES_GROUPES', numgr), 'E', jnsg)
        ii = 0
        do 34, iocc = 1, nbsitu, 1
        call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbvg = -n1
            call wkvect('&&RC36SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis(motcl1, 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi(jnbvg),&
                        nbret=n1)
            do 341 ing = 1, nbvg
                if (zi(jnbvg+ing-1) .eq. numgr) then
                    ii = ii + 1
                    zi(jnsg+ii-1) = iocc
                endif
341          continue
            call jedetr('&&RC36SI.VALE_GR')
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
                call u2mesi('F', 'POSTRCCM_26', 3, vali)
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
        call jecroc(jexnum('&&RC3600.LES_GROUPES', nbgr))
        call jeecra(jexnum('&&RC3600.LES_GROUPES', nbgr), 'LONMAX', ndim)
        call jeveuo(jexnum('&&RC3600.LES_GROUPES', nbgr), 'E', jnsg)
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
        call jeecra(jexnum('&&RC3600.LES_GROUPES', nbgr), 'LONUTI', ii)
    endif
!
    call jedetr('&&RC36SI.NUME_GROUP')
!
end subroutine
