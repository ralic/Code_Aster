subroutine rc32si()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/as_allocate.h"
#include "asterfort/wkvect.h"
#include "asterfort/jecrec.h"
#include "asterfort/jeecra.h"
#include "asterfort/codent.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
#include "asterfort/jedetr.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecroc.h"
#include "asterfort/jexnom.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ordis.h"
#include "asterfort/jexnum.h"
#include "asterfort/as_deallocate.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE B3200 et ZE200
!     RECUPERATION DES DONNEES DU MOT CLE "SITUATION"
!     ------------------------------------------------------------------
    integer :: nbsitu, nbseis, ndim
    integer, pointer :: nume_group(:) => null()
    integer :: jsigr, jnsitu, jnbocc, jpresa, jpresb, jcombi
    integer :: jsp12, jsp23, jsp13, nbgr, iocc, nume, n1, nocc
    aster_logical :: yapass
    character(len=8) :: k8b, knume
    integer :: nbvg, jnbvg, ing, numgr, ig, numpas(2)
    character(len=8) :: ouinon
    integer :: nbchar, jchara, jcharb, nbth, jther, nbpres, jpres
    integer :: nbmec, jmec, nscy, jnumgr, jseigr, nbgrt, nbsigr
    integer :: numgs, jnsg, ii, vali(3), jspas, nbsg1, nbsg2, nbsg3
    integer :: nbp12, nbp23, nbp13, numg1, numg2
!
! DEB ------------------------------------------------------------------
!
    call getfac('SITUATION', nbsitu)
    call getfac('SEISME', nbseis)
    ndim = nbsitu + nbseis
    AS_ALLOCATE(vi=nume_group, size=ndim)
!
    call wkvect('&&RC32SI.SITU_GROUP', 'V V I', 2*ndim, jsigr)
    call wkvect('&&RC3200.SITU_NUMERO', 'V V I', ndim, jnsitu)
    call wkvect('&&RC3200.SITU_NB_OCCUR', 'V V I', 2*ndim, jnbocc)
    call wkvect('&&RC3200.SITU_PRES_A', 'V V R', nbsitu, jpresa)
    call wkvect('&&RC3200.SITU_PRES_B', 'V V R', nbsitu, jpresb)
    call wkvect('&&RC3200.SITU_COMBINABLE', 'V V L', ndim, jcombi)
!
    call jecrec('&&RC3200.SITU_ETAT_A', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                ndim)
    call jecrec('&&RC3200.SITU_ETAT_B', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsitu)
!
    call jecrec('&&RC3200.SITU_THER', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                ndim)
    call jecrec('&&RC3200.SITU_PRES', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                ndim)
    call jecrec('&&RC3200.SITU_MECA', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                ndim)
!
    call wkvect('&&RC32SI.PASSAGE_1_2', 'V V I', ndim, jsp12)
    call wkvect('&&RC32SI.PASSAGE_2_3', 'V V I', ndim, jsp23)
    call wkvect('&&RC32SI.PASSAGE_1_3', 'V V I', ndim, jsp13)
    call jeecra('&&RC32SI.PASSAGE_1_2', 'LONUTI', 0)
    call jeecra('&&RC32SI.PASSAGE_2_3', 'LONUTI', 0)
    call jeecra('&&RC32SI.PASSAGE_1_3', 'LONUTI', 0)
!
!-- Nombre de groupes au total
    nbgr = 0
!-- Y-a-t-il une situation de passage ?
    yapass = .false.
!
!-----------------------------------------------
!------ BOUCLE SUR LE MOT CLE SITUATION
!-----------------------------------------------
    do 10 iocc = 1, nbsitu, 1
!
        call codent(iocc, 'D0', k8b)
!
! ------ ON STOCKE LE NUMERO DE SITUATION:
!        ---------------------------------
!
        call getvis('SITUATION', 'NUME_SITU', iocc=iocc, scal=nume, nbret=n1)
        zi(jnsitu+iocc-1) = nume
        knume = 'S       '
        call codent(nume, 'D0', knume(2:8))
!
! ------ ON STOCKE LE NOMBRE D'OCCURRENCE:
!        ---------------------------------
!
        call getvis('SITUATION', 'NB_OCCUR', iocc=iocc, scal=nocc, nbret=n1)
        zi(jnbocc+2*iocc-2) = nocc
!
! ------ ON STOCKE LES PRESSIONS: (0 si b3200_t ou ze200b)
!        ------------------------
!
        call getvr8('SITUATION', 'PRES_A', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvr8('SITUATION', 'PRES_A', iocc=iocc, scal=zr(jpresa+iocc-1), nbret=n1)
        else
            zr(jpresa+iocc-1)= 0.d0
        endif
        call getvr8('SITUATION', 'PRES_B', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvr8('SITUATION', 'PRES_B', iocc=iocc, scal=zr(jpresb+iocc-1), nbret=n1)
        else
            zr(jpresb+iocc-1)= 0.d0
        endif
!
! ------ ON STOCKE LE OU LES NUMEROS DE GROUPE:
!        --------------------------------------
!
        call getvis('SITUATION', 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
        nbvg = -n1
        if(nbvg .eq. 1 .or. nbvg .eq. 2) then
            call wkvect('&&RC32SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis('SITUATION', 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi( jnbvg),&
                        nbret=n1)
            do 26 ing = 1, nbvg
                numgr = zi(jnbvg+ing-1)
!-------------- On vérifie que le numéro de groupe est strictement positif
                if (numgr .le. 0) then
                call utmess('F', 'POSTRCCM_12')
                endif
!-------------- On comptabilise le nombre de groupes au total nbgr
                do 20 ig = 1, nbgr
                    if (nume_group(ig) .eq. numgr) goto 21
 20             continue
                nbgr = nbgr + 1
                nume_group(nbgr) = numgr
 21             continue
 26         continue
!
!----------- Si la situation n'appartient qu'à un seul groupe
            if (nbvg .eq. 1) then
                zi(jsigr+2*iocc-2) = zi(jnbvg)
                zi(jsigr+2*iocc-1) = zi(jnbvg)
            else
!----------- Sinon (elle appartient à deux groupes)
                zi(jsigr+2*iocc-2) = zi(jnbvg)
                zi(jsigr+2*iocc-1) = zi(jnbvg+1)
            endif
            call jedetr('&&RC32SI.VALE_GR')
        else
            call utmess('F', 'POSTRCCM_12')
        endif
!------- Si c'est une situation de passage
        call getvis('SITUATION', 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            yapass = .true.
            call getvis('SITUATION', 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
                        nbret=n1)
            if (numpas(1) .le. 0 .or. numpas(2) .le. 0 ) then
                call utmess('F', 'POSTRCCM_34')
            endif
            if (numpas(1) .gt. 3 .or. numpas(2) .gt. 3) then
                call utmess('F', 'POSTRCCM_34')
            endif
!
            zi(jsigr+2*iocc-2) = min ( numpas(1), numpas(2) )
            zi(jsigr+2*iocc-1) = max ( numpas(1), numpas(2) )
!
!----------- On comptabilise le nombre de groupes au total nbgr
            numgr = numpas(1)
            do 22 ig = 1, nbgr
                if (nume_group(ig) .eq. numgr) goto 23
 22         continue
            nbgr = nbgr + 1
            nume_group(nbgr) = numgr
 23         continue
            numgr = numpas(2)
            do 24 ig = 1, nbgr
                if (nume_group(ig) .eq. numgr) goto 25
 24         continue
            nbgr = nbgr + 1
            nume_group(nbgr) = numgr
 25         continue
!
        endif
!
! ------ EST-ELLE COMBINABLE DANS SON GROUPE:
!        ------------------------------------
        call getvtx('SITUATION', 'COMBINABLE', iocc=iocc, scal=ouinon, nbret=n1)
        if (ouinon(1:3) .eq. 'OUI') then
            zl(jcombi+iocc-1) = .true.
        else
            zl(jcombi+iocc-1) = .false.
        endif
!
! ------ ETAT DE CHARGEMENT POUR "A":
!        ----------------------------
        call getvis('SITUATION', 'CHAR_ETAT_A', iocc=iocc, nbval=0, nbret=n1)
        nbchar = -n1
!------ pour l'option B3200_T nbchar=0 sinon nbchar=1
        call jecroc(jexnom('&&RC3200.SITU_ETAT_A', knume))
        call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONMAX', nbchar)
        call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONUTI', nbchar)
        if (nbchar .ne. 0) then
            call jeveuo(jexnom('&&RC3200.SITU_ETAT_A', knume), 'E', jchara)
            call getvis('SITUATION', 'CHAR_ETAT_A', iocc=iocc, nbval=nbchar, scal=zi(jchara),&
                        nbret=n1)
        endif
!
! ------ ETAT DE CHARGEMENT POUR "B":
!        ----------------------------
        call getvis('SITUATION', 'CHAR_ETAT_B', iocc=iocc, nbval=0, nbret=n1)
        nbchar = -n1
!------ pour l'option B3200_T nbchar=0 sinon nbchar=1
        call jecroc(jexnom('&&RC3200.SITU_ETAT_B', knume))
        call jeecra(jexnom('&&RC3200.SITU_ETAT_B', knume), 'LONMAX', nbchar)
        call jeecra(jexnom('&&RC3200.SITU_ETAT_B', knume), 'LONUTI', nbchar)
        if (nbchar .ne. 0) then
            call jeveuo(jexnom('&&RC3200.SITU_ETAT_B', knume), 'E', jcharb)
            call getvis('SITUATION', 'CHAR_ETAT_B', iocc=iocc, nbval=nbchar, scal=zi(jcharb),&
                        nbret=n1)
        endif
!
! ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
!        ---------------------------------------------
        call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, nbval=0, nbret=n1)
        nbth = -n1
        call jecroc(jexnom('&&RC3200.SITU_THER', knume))
        call jeecra(jexnom('&&RC3200.SITU_THER', knume), 'LONMAX', nbth)
        call jeecra(jexnom('&&RC3200.SITU_THER', knume), 'LONUTI', nbth)
!
        if (nbth .ne. 0) then
            call jeveuo(jexnom('&&RC3200.SITU_THER', knume), 'E', jther)
            call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, nbval=nbth, scal=zi(jther),&
                        nbret=n1)
        endif
!
! ------ TRANSITOIRE DE PRESSION ASSOCIE A LA SITUATION:
!        ---------------------------------------------
        call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, nbval=0, nbret=n1)
        nbpres = -n1
        call jecroc(jexnom('&&RC3200.SITU_PRES', knume))
        call jeecra(jexnom('&&RC3200.SITU_PRES', knume), 'LONMAX', nbpres)
        call jeecra(jexnom('&&RC3200.SITU_PRES', knume), 'LONUTI', nbpres)
!
        if (nbpres .ne. 0) then
            call jeveuo(jexnom('&&RC3200.SITU_PRES', knume), 'E', jpres)
            call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, nbval=nbpres, scal=zi(jpres),&
                        nbret=n1)
        endif
!
! ------ TRANSITOIRE MECANIQUE ASSOCIE A LA SITUATION:
!        ---------------------------------------------
        call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, nbval=0, nbret=n1)
        nbmec = -n1
        call jecroc(jexnom('&&RC3200.SITU_MECA', knume))
        call jeecra(jexnom('&&RC3200.SITU_MECA', knume), 'LONMAX', nbmec)
        call jeecra(jexnom('&&RC3200.SITU_MECA', knume), 'LONUTI', nbmec)
!
        if (nbmec .ne. 0) then
            call jeveuo(jexnom('&&RC3200.SITU_MECA', knume), 'E', jmec)
            call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, nbval=nbmec, scal=zi(jmec),&
                        nbret=n1)
        endif
 10 end do
!
!-----------------------------------------------
!------- BOUCLE SUR LE MOT CLE SEISME
!-----------------------------------------------
    do 110 iocc = 1, nbseis, 1
!
! ------ LE NUMERO DE GROUPE:
!        -----------------------
        call getvis('SEISME', 'NUME_GROUPE', iocc=iocc, scal=nume, nbret=n1)
        zi(jsigr+2*(nbsitu+iocc)-2) = nume
        zi(jsigr+2*(nbsitu+iocc)-1) = nume
!
! ------ COMBINABLE DANS SON GROUPE:
!        ------------------------------------
        zl(jcombi+nbsitu+iocc-1) = .true.
!
! ------ LE NUMERO DE SITUATION:
!        -----------------------
        call getvis('SEISME', 'NUME_SITU', iocc=iocc, scal=nume, nbret=n1)
        zi(jnsitu+nbsitu+iocc-1) = nume
        knume = 'S       '
        call codent(nume, 'D0', knume(2:8))
!
! ------ LE NOMBRE D'OCCURRENCE DE SEISME:
!        ---------------------------------
        call getvis('SEISME', 'NB_OCCUR', iocc=iocc, scal=nocc, nbret=n1)
        zi(jnbocc+2*(nbsitu+iocc)-2) = nocc
!
! ------ LE NOMBRE DE CYCLES ASSOCIES:
!        ---------------------------------
        call getvis('SEISME', 'NB_CYCL_SEISME', iocc=iocc, scal=nscy, nbret=n1)
        zi(jnbocc+2*(nbsitu+iocc)-1) = nscy
!
! ------ TRANSITOIRES MECANIQUES ASSOCIES AU SEISME:
!        ---------------------------------------------
        call getvis('SEISME', 'CHAR_ETAT', iocc=iocc, nbval=0, nbret=n1)
        nbchar = -n1
        call jecroc(jexnom('&&RC3200.SITU_ETAT_A', knume))
        call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONMAX', nbchar)
        call jeecra(jexnom('&&RC3200.SITU_ETAT_A', knume), 'LONUTI', nbchar)
        call jeveuo(jexnom('&&RC3200.SITU_ETAT_A', knume), 'E', jchara)
        call getvis('SEISME', 'CHAR_ETAT', iocc=iocc, nbval=nbchar, vect=zi(jchara),&
                    nbret=n1)
!
! ------ PAS DE TRANSITOIRE THERMIQUE NI DE PRESSION
!        ---------------------------------------------
        call jecroc(jexnom('&&RC3200.SITU_THER', knume))
        call jeecra(jexnom('&&RC3200.SITU_THER', knume), 'LONMAX', 0)
        call jeecra(jexnom('&&RC3200.SITU_THER', knume), 'LONUTI', 0)
!
        call jecroc(jexnom('&&RC3200.SITU_PRES', knume))
        call jeecra(jexnom('&&RC3200.SITU_PRES', knume), 'LONMAX', 0)
        call jeecra(jexnom('&&RC3200.SITU_PRES', knume), 'LONUTI', 0)     
!
110 end do
!
!-----------------------------------------------
!------- DEFINITION DES GROUPES
!-----------------------------------------------
    call ordis(nume_group, nbgr)
!
! -- incompatibilité des situations de passage et de plus de 3 groupes
    if (nbgr .gt. 3 .and. yapass) then
        call utmess('F', 'POSTRCCM_34')
    endif
!
! --- on ajoute un groupe pour les situations de passage
    if (yapass) nbgr = nbgr + 1
!
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
!
    do 30 ig = 1, nbgrt, 1
!
        numgr = nume_group(ig)
        zi(jnumgr+ig-1) = numgr
!
! ------ on compte le nombre de situations dans le groupe
        nbsigr = 0
        do 32 iocc = 1, nbsitu, 1
            call getvis('SITUATION', 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
            nbvg = -n1
            call wkvect('&&RC32SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis('SITUATION', 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi(jnbvg),&
                        nbret=n1)
            do 321 ing = 1, nbvg
                if (zi(jnbvg+ing-1) .eq. numgr) nbsigr = nbsigr + 1
321         continue
            call jedetr('&&RC32SI.VALE_GR')
!
            call getvis('SITUATION', 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvis('SITUATION', 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
                            nbret=n1)
                if (numpas(1) .eq. numgr) nbsigr = nbsigr + 1
                if (numpas(2) .eq. numgr) nbsigr = nbsigr + 1
            endif
 32     continue
!
! ------ on ajoute le séisme à ce nombre si présent
!
        do 36 iocc = 1, nbseis, 1
            call getvis('SEISME', 'NUME_GROUPE', iocc=iocc, scal=numgs, nbret=n1)
            if (numgs .eq. numgr) nbsigr = nbsigr + 1
 36     continue
!
! ------ pour chaque groupe, on stocke ses numéros de situations
! ------ (normales, passages et séisme)
        call jecroc(jexnum('&&RC3200.LES_GROUPES', numgr))
        call jeecra(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONMAX', nbsigr)
        call jeecra(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONUTI', nbsigr)
        call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'E', jnsg)
!
        ii = 0
        do 34 iocc = 1, nbsitu, 1
            call getvis('SITUATION', 'NUME_GROUPE', iocc=iocc, nbval=0, nbret=n1)
            nbvg = -n1
            call wkvect('&&RC32SI.VALE_GR', 'V V I', nbvg, jnbvg)
            call getvis('SITUATION', 'NUME_GROUPE', iocc=iocc, nbval=nbvg, vect=zi(jnbvg),&
                        nbret=n1)
            do 341 ing = 1, nbvg
                if (zi(jnbvg+ing-1) .eq. numgr) then
                    ii = ii + 1
                    zi(jnsg+ii-1) = iocc
                endif
341         continue
            call jedetr('&&RC32SI.VALE_GR')
!
            call getvis('SITUATION', 'NUME_PASSAGE', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvis('SITUATION', 'NUME_PASSAGE', iocc=iocc, nbval=2, vect=numpas,&
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
 34     continue
!
        do 38 iocc = 1, nbseis, 1
            call getvis('SEISME', 'NUME_GROUPE', iocc=iocc, scal=numgs, nbret=n1)
            if (numgs .eq. numgr) then
                ii = ii + 1
                zi(jnsg+ii-1) = nbsitu+iocc
!-------------- un seul séisme par groupe
                if (zi(jseigr+ig-1) .ne. 0) then
                    vali(1) = numgr
                    vali(2) = iocc
                    vali(3) = zi(jseigr+ig-1)
                    call utmess('F', 'POSTRCCM_26', ni=3, vali=vali)
                endif
                zi(jseigr+ig-1) = iocc
            endif
 38     continue
!
 30 end do
!
!-----------------------------------------------
!------- TRAITEMENT DES SITUATIONS DE PASSAGE
!-----------------------------------------------
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
!
        do 40 iocc = 1, ndim, 1
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
 40     continue
!
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
        do 42 iocc = 1, ndim, 1
            numg1 = zi(jsigr+2*iocc-2)
            numg2 = zi(jsigr+2*iocc-1)
            if (numg1 .eq. 1 .and. numg2 .eq. 1) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
 42     continue
        do 44 iocc = 1, ndim, 1
            numg1 = zi(jsigr+2*iocc-2)
            numg2 = zi(jsigr+2*iocc-1)
            if (numg1 .eq. 1 .and. numg2 .eq. 2) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
            if (numg1 .eq. 2 .and. numg2 .eq. 2) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
            if (numg1 .eq. 2 .and. numg2 .eq. 3) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
            if (numg1 .eq. 3 .and. numg2 .eq. 3) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
            if (numg1 .eq. 1 .and. numg2 .eq. 3) then
                ii = ii + 1
                zi(jnsg+ii-1) = iocc
            endif
 44     continue
        call jeecra(jexnum('&&RC3200.LES_GROUPES', nbgr), 'LONUTI', ii)
    endif
!
    AS_DEALLOCATE(vi=nume_group)
!
end subroutine
