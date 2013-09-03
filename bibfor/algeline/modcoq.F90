subroutine modcoq(base, nuor, nbm, mater1, mater2,&
                  noma, nomgrp, iaxe, kec, geom,&
                  vicoq, torco, tcoef, ifreba)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! CARACTERISATION DES DEFORMEES DES MODES PRIS EN COMPTE POUR LE
! COUPLAGE : POUR CHAQUE MODE ET SUR CHACUNE DES DEUX COQUES,
! DETERMINATION DE L'ORDRE DE COQUE ET DES COEFFICIENTS DE LA DEFORMEE
! AXIALE DE POUTRE
! APPELANT : FLUST4
!-----------------------------------------------------------------------
!  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
!                MODALE
!  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES PRIS EN COMPTE POUR
!                LE COUPLAGE FLUIDELASTIQUE
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : MATER1 : NOM DU CONCEPT DE TYPE MATER (MATERIAU COQUE INTERNE)
!  IN : MATER2 : NOM DU CONCEPT DE TYPE MATER (MATERIAU COQUE EXTERNE)
!  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
!  IN : NOMGRP : LISTE DES NOMS DES GROUPES DE NOEUDS/GROUPES DE MAILLES
!                CORRESPONDANT AUX COQUES (LES GROUPES DE NOEUDS ONT ETE
!                PREALABLEMENT CREES A PARTIR DES GROUPES DE MAILLES ET
!                ON LEUR A AFFECTE LES MEMES NOMS)
!  IN : IAXE   : INDICE CARACTERISANT L'AXE DE REVOLUTION DES COQUES
!                IAXE = 1 : AXE X DU REPERE GLOBAL
!                IAXE = 2 : AXE Y DU REPERE GLOBAL
!                IAXE = 3 : AXE Z DU REPERE GLOBAL
!  IN : KEC    : INDICE CARACTERISTIQUE DU SENS DE L'ECOULEMENT
!                KEC =  1 : ECOULEMENT DANS LE SENS CROISSANT DU
!                PARAMETRE LE LONG DE L'AXE DE REVOLUTION DES COQUES
!                KEC = -1 : ECOULEMENT DANS LE SENS DECROISSANT
!  IN : GEOM   : VECTEUR DE GRANDEURS GEOMETRIQUES CARACTERISTIQUES
! OUT : VICOQ  : VECTEUR D'INDICES CARACTERISANT CHAQUE MODE
!                VICOQ(IMOD) = 1 => COQUE INTERNE SEULE EN MVT
!                VICOQ(IMOD) = 2 => COQUE EXTERNE SEULE EN MVT
!                VICOQ(IMOD) = 3 => COQUES INTERNE + EXTERNE EN MVT
! OUT : TORCO  : TABLEAU CONTENANT LES ORDRES DE COQUE ET DEPHASAGES
! OUT : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/coedef.h"
#include "asterfort/dismoi.h"
#include "asterfort/extmod.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ordcoq.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: base, mater1, mater2, noma
    integer :: nbm, nuor(nbm), iaxe, kec, vicoq(nbm)
    real(kind=8) :: geom(9), torco(4, nbm), tcoef(10, nbm)
!
    integer :: iddl(2)
    character(len=3) :: kmod
    character(len=8) :: k8bid, nompar(3)
    character(len=14) :: numddl
    character(len=19) :: nomrc
    character(len=24) :: matria, coorno, rcvalk, rcvalr, nomgrp(*)
    character(len=32) :: grpno
!
!-----------------------------------------------------------------------
    integer :: ibi, icoor, icoq, idec, idecm, idecmn, idefm
    integer :: ifm, ifreba, imod, inmaxe, inmaxi, ino, inunoe
    integer :: inunoi,iok1,iok2,iok3,ipara,iret
    integer :: ivalk, ivalr, nbeq, nbnoex, nbnoin, nbnoto
    integer :: nbpara, numnoe, numod, nunoe0
    real(kind=8) :: dpmaxe, dpmaxi, dpnorm, drmax, dx1, dx2, fremod
    real(kind=8) :: poiss1, poiss2, rho1, rho2, rtemp, tole, young1
    real(kind=8) :: young2
!-----------------------------------------------------------------------
    data nompar /'E       ','NU      ','RHO     '/
!
!-----------------------------------------------------------------------
    call jemarq()
!
!
! --- 1.INITIALISATIONS ET FORMATS D'IMPRESSION
!
    tole = 1.d-4
    if (iaxe .eq. 1) then
        iddl(1) = 2
        iddl(2) = 3
    else if (iaxe.eq.2) then
        iddl(1) = 3
        iddl(2) = 1
    else
        iddl(1) = 1
        iddl(2) = 2
    endif
!
!
    ifm = iunifi('MESSAGE')
    500 format('*******************************************')
    501 format('*                                         *')
    502 format('*  CARACTERISATION DES DEFORMEES MODALES  *')
    503 format('DEFORMEE DU ',i3,'-IEME MODE RETENU')
    504 format('================================')
    510 format('-----  MOUVEMENT DE LA COQUE INTERNE  ------')
    511 format('--- PAS DE MOUVEMENT DE LA COQUE INTERNE ---')
    520 format('-----  MOUVEMENT DE LA COQUE EXTERNE  ------')
    521 format('--- PAS DE MOUVEMENT DE LA COQUE EXTERNE ---')
    530 format(30x,'---/---')
!
!
! --- 2.RECUPERATION DES CARACTERISTIQUES MATERIAU
!
! --- 2.1.MATERIAU CONSTITUTIF DE LA COQUE INTERNE
!
    nomrc = mater1//'.ELAS      '
    rcvalk = nomrc//'.VALK'
    rcvalr = nomrc//'.VALR'
    call jeexin(rcvalk, iret)
    if (iret .eq. 0) call u2mess('F', 'ALGELINE_92')
    call jeveuo(rcvalk, 'L', ivalk)
    call jeveuo(rcvalr, 'L', ivalr)
!
    iok1 = 0
    iok2 = 0
    iok3 = 0
    call jelira(rcvalr, 'LONUTI', nbpara)
    do 10 ipara = 1, nbpara
        if (zk8(ivalk+ipara-1) .eq. nompar(1)) then
            iok1 = 1
            young1 = zr(ivalr+ipara-1)
        else if (zk8(ivalk+ipara-1).eq.nompar(2)) then
            iok2 = 1
            poiss1 = zr(ivalr+ipara-1)
        else if (zk8(ivalk+ipara-1).eq.nompar(3)) then
            iok3 = 1
            rho1 = zr(ivalr+ipara-1)
        endif
10  continue
    if (iok1 .eq. 0 .or. iok2 .eq. 0 .or. iok3 .eq. 0) then
        call u2mess('F', 'ALGELINE_93')
    else if (young1.eq.0.d0) then
        call u2mess('F', 'ALGELINE_94')
    endif
!
! --- 2.2.MATERIAU CONSTITUTIF DE LA COQUE EXTERNE
!
    nomrc = mater2//'.ELAS      '
    rcvalk = nomrc//'.VALK'
    rcvalr = nomrc//'.VALR'
    call jeexin(rcvalk, iret)
    if (iret .eq. 0) call u2mess('F', 'ALGELINE_95')
    call jeveuo(rcvalk, 'L', ivalk)
    call jeveuo(rcvalr, 'L', ivalr)
!
    iok1 = 0
    iok2 = 0
    iok3 = 0
    call jelira(rcvalr, 'LONUTI', nbpara)
    do 20 ipara = 1, nbpara
        if (zk8(ivalk+ipara-1) .eq. nompar(1)) then
            iok1 = 1
            young2 = zr(ivalr+ipara-1)
        else if (zk8(ivalk+ipara-1).eq.nompar(2)) then
            iok2 = 1
            poiss2 = zr(ivalr+ipara-1)
        else if (zk8(ivalk+ipara-1).eq.nompar(3)) then
            iok3 = 1
            rho2 = zr(ivalr+ipara-1)
        endif
20  continue
    if (iok1 .eq. 0 .or. iok2 .eq. 0 .or. iok3 .eq. 0) then
        call u2mess('F', 'ALGELINE_96')
    else if (young2.eq.0.d0) then
        call u2mess('F', 'ALGELINE_97')
    endif
!
!
! --- 3.EXTRACTION DES DEFORMEES MODALES DANS LES DEUX DIRECTIONS DU
! ---   PLAN ORTHOGONAL A L'AXE DE REVOLUTION DES COQUES
!
    call dismoi('F', 'REF_RIGI_PREM', base, 'RESU_DYNA', ibi, matria, iret)
!
    call dismoi('F', 'NOM_NUME_DDL', matria, 'MATR_ASSE', ibi,&
                numddl, iret)
    call dismoi('F', 'NB_EQUA', matria, 'MATR_ASSE', nbeq,&
                k8bid, iret)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoto,&
                k8bid, iret)
!
    call wkvect('&&MODCOQ.TEMP.DEFM', 'V V R', 2*nbnoto*nbm, idefm)
    call extmod(base, numddl, nuor, nbm, zr(idefm),&
                nbeq, nbnoto, iddl, 2)
!
!.....PERMUTATION DES DDLS DX, DZ -> DZ, DX LORSQUE IAXE = 2
!.....(LE TRIEDRE Z, X, Y EST DIRECT)
!
    if (iaxe .eq. 2) then
        do 30 imod = 1, nbm
            idecm = 2 * nbnoto * (imod-1)
            do 31 ino = 1, nbnoto
                idecmn = idecm + 2 * (ino-1)
                rtemp = zr(idefm+idecmn)
                zr(idefm+idecmn) = zr(idefm+idecmn+1)
                zr(idefm+idecmn+1) = rtemp
31          continue
30      continue
    endif
!
!
! --- 4.ACCES AUX OBJETS DU CONCEPT MAILLAGE
!
!     GROUPE DE NOEUDS DE LA COQUE INTERNE
    grpno = '&&MEFGMN.00000001       '
    call jelira(grpno, 'LONMAX', nbnoin)
    call jeveuo(grpno, 'L', inunoi)
!     GROUPE DE NOEUDS DE LA COQUE EXTERNE
    grpno = '&&MEFGMN.00000002       '
    call jelira(grpno, 'LONMAX', nbnoex)
    call jeveuo(grpno, 'L', inunoe)
!
    coorno = noma//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', icoor)
!
!
! --- 5.POUR CHAQUE MODE ET SUR CHACUNE DES DEUX COQUES
!       - DETECTION DU NOEUD DE DEPLACEMENT MAXIMUM DANS LE PLAN
!         PERPENDICULAIRE A L'AXE DE REVOLUTION
!       - DETERMINATION DE L'ORDRE DE COQUE ET DU DEPHASAGE
!       - DETERMINATION DES COEFFICIENTS DE LA DEFORMEE AXIALE
!
    write(ifm,500)
    write(ifm,501)
    write(ifm,502)
    write(ifm,501)
    write(ifm,500)
    write(ifm,*)
!
!
    do 40 imod = 1, nbm
!
        write(ifm,503) imod
        write(ifm,504)
        write(ifm,*)
        numod = nuor(imod)
        fremod = zr(ifreba+numod-1)
!
! ----- 5.1.DETECTION DU DEPLACEMENT MAXIMUM SUR LA COQUE INTERNE
!
        dpmaxi = 0.d0
        inmaxi = 1
        do 50 ino = 1, nbnoin
            numnoe = zi(inunoi+ino-1)
            idec = 2*nbnoto*(imod-1)+2*(numnoe-1)
            dx1 = zr(idefm+idec)
            dx2 = zr(idefm+idec+1)
            dpnorm = dble(sqrt(dx1*dx1+dx2*dx2))
            if (dpnorm .gt. dpmaxi) then
                dpmaxi = dpnorm
                inmaxi = ino
            endif
50      continue
!
! ----- 5.2.DETECTION DU DEPLACEMENT MAXIMUM SUR LA COQUE EXTERNE
!
        dpmaxe = 0.d0
        inmaxe = 1
        do 60 ino = 1, nbnoex
            numnoe = zi(inunoe+ino-1)
            idec = 2*nbnoto*(imod-1)+2*(numnoe-1)
            dx1 = zr(idefm+idec)
            dx2 = zr(idefm+idec+1)
            dpnorm = dble(sqrt(dx1*dx1+dx2*dx2))
            if (dpnorm .gt. dpmaxe) then
                dpmaxe = dpnorm
                inmaxe = ino
            endif
60      continue
!
! ----- 5.3.DETERMINATION DE L'ORDRE DE COQUE ET DES COEFFICIENTS DE
! -----     LA DEFORMEE AXIALE
!
! ----- 5.3.1.COQUE INTERNE SEULE EN MOUVEMENT
!
        if (dpmaxe .lt. dpmaxi*tole) then
!
            write(ifm,510)
            write(ifm,*)
            icoq = 1
            call ordcoq(imod, nbm, icoq, nbnoin, zi(inunoi),&
                        inmaxi, nbnoto, zr(icoor), iaxe, zr(idefm),&
                        nunoe0, drmax, torco)
            call coedef(imod, fremod, nbm, young1, poiss1,&
                        rho1, icoq, nbnoin, zi(inunoi), nunoe0,&
                        nbnoto, zr(icoor), iaxe, kec, geom,&
                        zr(idefm), drmax, torco, tcoef)
!
            vicoq(imod) = icoq
            write(ifm,*)
            write(ifm,521)
            write(ifm,*)
!
! ----- 5.3.2.COQUE EXTERNE SEULE EN MOUVEMENT
!
        else if (dpmaxi.lt.dpmaxe*tole) then
!
            write(ifm,511)
            write(ifm,*)
            write(ifm,520)
            write(ifm,*)
            icoq = 2
            call ordcoq(imod, nbm, icoq, nbnoex, zi(inunoe),&
                        inmaxe, nbnoto, zr(icoor), iaxe, zr(idefm),&
                        nunoe0, drmax, torco)
            call coedef(imod, fremod, nbm, young2, poiss2,&
                        rho2, icoq, nbnoex, zi(inunoe), nunoe0,&
                        nbnoto, zr(icoor), iaxe, kec, geom,&
                        zr(idefm), drmax, torco, tcoef)
!
            vicoq(imod) = icoq
            write(ifm,*)
!
! ----- 5.3.3.COQUES INTERNE + EXTERNE EN MOUVEMENT
!
        else
!
            write(kmod,'(I3)') imod
            call u2mesk('A', 'ALGELINE_98', 1, kmod)
!
            write(ifm,510)
            write(ifm,*)
            icoq = 1
            call ordcoq(imod, nbm, icoq, nbnoin, zi(inunoi),&
                        inmaxi, nbnoto, zr(icoor), iaxe, zr(idefm),&
                        nunoe0, drmax, torco)
            call coedef(imod, fremod, nbm, young1, poiss1,&
                        rho1, icoq, nbnoin, zi(inunoi), nunoe0,&
                        nbnoto, zr(icoor), iaxe, kec, geom,&
                        zr(idefm), drmax, torco, tcoef)
!
            write(ifm,*)
            write(ifm,520)
            write(ifm,*)
            icoq = 2
            call ordcoq(imod, nbm, icoq, nbnoex, zi(inunoe),&
                        inmaxe, nbnoto, zr(icoor), iaxe, zr(idefm),&
                        nunoe0, drmax, torco)
            call coedef(imod, fremod, nbm, young2, poiss2,&
                        rho2, icoq, nbnoex, zi(inunoe), nunoe0,&
                        nbnoto, zr(icoor), iaxe, kec, geom,&
                        zr(idefm), drmax, torco, tcoef)
!
            vicoq(imod) = 3
            write(ifm,*)
!
        endif
!
        write(ifm,530)
        write(ifm,*)
!
40  continue
!
! --- MENAGE
    call jedetr('&&MODCOQ.TEMP.DEFM')
    call jedema()
!
end subroutine
