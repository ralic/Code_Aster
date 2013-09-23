subroutine chmima(nomsd, nomsy, typmax, nocham)
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/posddl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbordr
    character(len=*) :: nomsd, nomsy, typmax, nocham
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!      AFFECTATION DU CHAMP-GD DE NOM NOCHAM  AVEC LES
!      VALEURS MINMAX EN TOUT POINT DES CHAMPS-GD DE TYPE
!      NOMSY DU RESULTAT DE NOM NOMSD
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP A CHERCHER.
! IN  : TYPMAX : TYPE D'OPERATION A EFFECTUER
! VAR : NOCHAM : NOM DU CHAMP CONTENANT LES MINMAX DES
!                CHAMPS DE TYPE NOMSY DU RESULTAT NOMSD.
!
! ----------------------------------------------------------------------
    character(len=4) :: ctyp
    character(len=8) :: typma, k8b, crit, noma, nomn, valeur
    character(len=19) :: prno, prn2
    character(len=16) :: noms2
    character(len=19) :: nocha2, chextr, knum
    character(len=24) :: nomnoe
    character(len=5) :: sufv
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iad, ib, ie, in, inoe, inumer
    integer :: iret, ivale, j, jddlx, jddly, jddlz, jdlrx
    integer :: jdlry, jdlrz, jordr, jvpnt, n2, nbnoe, nc
    integer :: neq, np, nvale
    real(kind=8) :: epsi, rs1, x, y, z
!-----------------------------------------------------------------------
    call jemarq()
    knum = '&&CHMIMA.NUME_ORDRE'
    noms2 = nomsy
    nocha2 = nocham
    typma = typmax
!
!     --- LECTURE DU MOT-CLE TYPE_RESU ---
!
    call getvtx(' ', 'TYPE_RESU', scal=valeur, nbret=n2)
!
!     --- RECUPERATION DES NUMEROS D'ORDRE ---
!
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
!
    call rsutnu(nomsd, ' ', 0, knum, nbordr,&
                epsi, crit, iret)
    if (nbordr .eq. 0) then
        call utmess('F', 'UTILITAI_23')
    endif
    call jeveuo(knum, 'L', jordr)
!
!     --- TRAITEMENT DU PREMIER NUMERO D'ORDRE ---
!
    call rsexch('F', nomsd, noms2, zi(jordr), chextr,&
                iret)
!
!      --- INITIALISATION DE NOCHAM AVEC CHEXTR ---
    call copisd('CHAMP_GD', 'G', chextr(1:19), nocha2(1:19))
!
!     --- RECUPERATION DES VALEURS DU CHAMP-GD ---
    call jeexin(nocha2(1:19)//'.VALE', iret)
    if (iret .gt. 0) then
        sufv= '.VALE'
    else
        sufv= '.CELV'
    endif
    call jelira(nocha2(1:19)//sufv, 'LONMAX', neq)
    call jeveuo(nocha2(1:19)//sufv, 'E', nvale)
!
    call wkvect('&&CHMIMA.INST', 'V V I', neq, inumer)
    do 1 i = 1, neq
        zi(inumer+i-1) = zi(jordr)
 1  end do
!
!     --- BOUCLE SUR LES NUMEROS D'ORDRE ---
!
    if (typma .eq. 'MAXI    ') then
!
        do 10 i = 2, nbordr
!
!         - RECUPERATION DU CHAMP DE TYPE NOMSY
!           CORRESPONDANT AU NUMERO D'ORDRE COURANT
!
            call rsexch('F', nomsd, noms2, zi(jordr+i-1), chextr,&
                        iret)
!
!         - RECUPERATION DU VALE DU CHAMP EXTRAIT
!
            call jeveuo(chextr//sufv, 'L', ivale)
!
            do 12 j = 1, neq
                if (zr(ivale+j-1) .gt. zr(nvale+j-1)) then
                    zr(nvale+j-1) = zr(ivale+j-1)
                    zi(inumer+j-1) = zi(jordr+i-1)
                endif
12          continue
!
10      continue
!
    else if (typma.eq.'MAXI_ABS') then
!
        do 20 i = 2, nbordr
!
!         - RECUPERATION DU CHAMP DE TYPE NOMSY
!           CORRESPONDANT AU NUMERO D'ORDRE COURANT
!
            call rsexch('F', nomsd, noms2, zi(jordr+i-1), chextr,&
                        iret)
!
!         - RECUPERATION DU VALE DU CHAMP EXTRAIT
!
            call jeveuo(chextr//sufv, 'L', ivale)
!
            do 22 j = 1, neq
!
                if (abs(zr(ivale+j-1)) .gt. abs(zr(nvale+j-1))) then
                    zr(nvale+j-1) = zr(ivale+j-1)
                    zi(inumer+j-1) = zi(jordr+i-1)
                endif
22          continue
!
20      continue
!
    else if (typma.eq.'MINI    ') then
!
        do 30 i = 2, nbordr
!
!         - RECUPERATION DU CHAMP DE TYPE NOMSY
!           CORRESPONDANT AU NUMERO D'ORDRE COURANT
!
            call rsexch('F', nomsd, noms2, zi(jordr+i-1), chextr,&
                        iret)
!
!         - RECUPERATION DU VALE DU CHAMP EXTRAIT
!
            call jeveuo(chextr//sufv, 'L', ivale)
!
            do 32 j = 1, neq
!
                if (zr(ivale+j-1) .lt. zr(nvale+j-1)) then
                    zr(nvale+j-1) = zr(ivale+j-1)
                    zi(inumer+j-1) = zi(jordr+i-1)
                endif
32          continue
!
30      continue
!
    else if (typma.eq.'MINI_ABS') then
!
        do 40 i = 2, nbordr
!
!         - RECUPERATION DU CHAMP DE TYPE NOMSY
!           CORRESPONDANT AU NUMERO D'ORDRE COURANT
!
            call rsexch('F', nomsd, noms2, zi(jordr+i-1), chextr,&
                        iret)
!
!         - RECUPERATION DU VALE DU CHAMP EXTRAIT
!
            call jeveuo(chextr//sufv, 'L', ivale)
!
            do 42 j = 1, neq
!
                if (abs(zr(ivale+j-1)) .lt. abs(zr(nvale+j-1))) then
                    zr(nvale+j-1) = zr(ivale+j-1)
                    zi(inumer+j-1) = zi(jordr+i-1)
                endif
42          continue
!
40      continue
!
    else if (typma.eq.'NORM_TRA') then
        call rsexch('F', nomsd, noms2, zi(jordr), chextr,&
                    iret)
        call jeveuo(chextr//'.VALE', 'L', ivale)
        call dismoi('F', 'PROF_CHNO', chextr, 'CHAM_NO', ib,&
                    prno, ie)
        call dismoi('F', 'NOM_MAILLA', chextr, 'CHAM_NO', ib,&
                    noma, ie)
        call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoe,&
                    k8b, ie)
        nomnoe = noma//'.NOMNOE'
!
        do 56 j = 0, neq-1
            zr(nvale+j) = zr(ivale+j)
56      continue
        if (nbordr .eq. 1) goto 58
!
        call wkvect('&&CHMIMA.DDL.DX', 'V V I', nbnoe, jddlx)
        call wkvect('&&CHMIMA.DDL.DY', 'V V I', nbnoe, jddly)
        call wkvect('&&CHMIMA.DDL.DZ', 'V V I', nbnoe, jddlz)
        call wkvect('&&CHMIMA.DDL.DRX', 'V V I', nbnoe, jdlrx)
        call wkvect('&&CHMIMA.DDL.DRY', 'V V I', nbnoe, jdlry)
        call wkvect('&&CHMIMA.DDL.DRZ', 'V V I', nbnoe, jdlrz)
        call wkvect('&&CHMIMA.VALE_P.NT', 'V V R', nbnoe, jvpnt)
!
        do 50 in = 0, nbnoe-1
            call jenuno(jexnum(nomnoe, in+1), nomn)
            call posddl('CHAM_NO', chextr, nomn, 'DX', inoe,&
                        zi(jddlx+in))
            call posddl('CHAM_NO', chextr, nomn, 'DY', inoe,&
                        zi(jddly+in))
            call posddl('CHAM_NO', chextr, nomn, 'DZ', inoe,&
                        zi(jddlz+in))
            call posddl('CHAM_NO', chextr, nomn, 'DRX', inoe,&
                        zi(jdlrx+in))
            call posddl('CHAM_NO', chextr, nomn, 'DRY', inoe,&
                        zi(jdlry+in))
            call posddl('CHAM_NO', chextr, nomn, 'DRZ', inoe,&
                        zi(jdlrz+in))
            x = zr(ivale+zi(jddlx+in)-1)
            y = zr(ivale+zi(jddly+in)-1)
            if (zi(jddlz+in) .ne. 0) then
                z = zr(ivale+zi(jddlz+in)-1)
            else
                z = 0.d0
            endif
            zr(jvpnt+in) = sqrt( x**2 + y**2 + z**2 )
            zr(nvale+zi(jddlx+in)-1) = x
            zr(nvale+zi(jddly+in)-1) = y
            if (zi(jddlz+in) .ne. 0) zr(nvale+zi(jddlz+in)-1) = z
            zi(inumer+zi(jddlx+in)-1) = zi(jordr)
            zi(inumer+zi(jddly+in)-1) = zi(jordr)
            zi(inumer+zi(jddlz+in)-1) = zi(jordr)
            if (zi(jdlrx+in) .ne. 0) zr(nvale+zi(jdlrx+in)-1) = zr( ivale+zi( jdlrx+in)-1 )
            if (zi(jdlry+in) .ne. 0) zr(nvale+zi(jdlry+in)-1) = zr( ivale+zi( jdlry+in)-1 )
            if (zi(jdlrz+in) .ne. 0) zr(nvale+zi(jdlrz+in)-1) = zr( ivale+zi( jdlrz+in)-1 )
50      continue
!
        do 52 i = 2, nbordr
            call rsexch('F', nomsd, noms2, zi(jordr+i-1), chextr,&
                        iret)
            call dismoi('F', 'PROF_CHNO', chextr, 'CHAM_NO', ib,&
                        prn2, ie)
            if (prn2 .ne. prno) then
                call utmess('F', 'UTILITAI_26')
            endif
            call jeveuo(chextr//'.VALE', 'L', ivale)
!
            do 54 in = 0, nbnoe-1
                x = zr(ivale+zi(jddlx+in)-1)
                y = zr(ivale+zi(jddly+in)-1)
                if (zi(jddlz+in) .ne. 0) then
                    z = zr(ivale+zi(jddlz+in)-1)
                else
                    z = 0.d0
                endif
                rs1 = sqrt( x**2 + y**2 + z**2 )
                if (rs1 .gt. zr(jvpnt+in)) then
                    zr(jvpnt+in) = rs1
                    zi(inumer+zi(jddlx+in)-1) = zi(jordr+i-1)
                    zi(inumer+zi(jddly+in)-1) = zi(jordr+i-1)
                    zi(inumer+zi(jddlz+in)-1) = zi(jordr+i-1)
                    zr(nvale+zi(jddlx+in)-1) = x
                    zr(nvale+zi(jddly+in)-1) = y
                    if (zi(jddlz+in) .ne. 0) zr(nvale+zi(jddlz+in)- 1) = z
                    if (zi(jdlrx+in) .ne. 0) zr(nvale+zi(jdlrx+in)- 1) = zr(ivale+zi(jdlrx+in)-1)
                    if (zi(jdlry+in) .ne. 0) zr(nvale+zi(jdlry+in)- 1) = zr(ivale+zi(jdlry+in)-1)
                    if (zi(jdlrz+in) .ne. 0) zr(nvale+zi(jdlrz+in)- 1) = zr(ivale+zi(jdlrz+in)-1)
                endif
54          continue
!
52      continue
        call jedetr('&&CHMIMA.VALE_P.NT')
!
    endif
!
58  continue
    if (valeur(1:4) .eq. 'INST') then
        if (typma .eq. 'NORM_TRA') then
            if (nbordr .ne. 1) then
                do 102 in = 0, nbnoe-1
                    call rsadpa(nomsd, 'L', 1, 'INST', zi(inumer+zi( jddlx+in)-1),&
                                0, sjv=iad, styp=ctyp)
                    zr(nvale+zi(jddlx+in)-1) = zr(iad)
                    zr(nvale+zi(jddly+in)-1) = zr(iad)
                    if (zi(jddlz+in) .ne. 0) zr(nvale+zi(jddlz+in)- 1) = zr(iad)
                    if (zi(jdlrx+in) .ne. 0) zr(nvale+zi(jdlrx+in)- 1) = zr(iad)
                    if (zi(jdlry+in) .ne. 0) zr(nvale+zi(jdlry+in)- 1) = zr(iad)
                    if (zi(jdlrz+in) .ne. 0) zr(nvale+zi(jdlrz+in)- 1) = zr(iad)
102              continue
                call jedetr('&&CHMIMA.DDL.DX')
                call jedetr('&&CHMIMA.DDL.DY')
                call jedetr('&&CHMIMA.DDL.DZ')
                call jedetr('&&CHMIMA.DDL.DRX')
                call jedetr('&&CHMIMA.DDL.DRY')
                call jedetr('&&CHMIMA.DDL.DRZ')
            else
                do 110 j = 0, neq-1
                    call rsadpa(nomsd, 'L', 1, 'INST', zi(inumer+j),&
                                0, sjv=iad, styp=ctyp)
                    zr(nvale+j) = zr(iad)
110              continue
            endif
        else
            do 120 j = 0, neq-1
                call rsadpa(nomsd, 'L', 1, 'INST', zi(inumer+j),&
                            0, sjv=iad, styp=ctyp)
                zr(nvale+j) = zr(iad)
120          continue
        endif
    else
        if (typma .eq. 'NORM_TRA') then
            call jedetr('&&CHMIMA.DDL.DX')
            call jedetr('&&CHMIMA.DDL.DY')
            call jedetr('&&CHMIMA.DDL.DZ')
            call jedetr('&&CHMIMA.DDL.DRX')
            call jedetr('&&CHMIMA.DDL.DRY')
            call jedetr('&&CHMIMA.DDL.DRZ')
        endif
    endif
!
    call jedetr('&&CHMIMA.INST')
    call jedetr(knum)
!
    call jedema()
end subroutine
