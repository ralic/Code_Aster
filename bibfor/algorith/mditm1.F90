subroutine mditm1(nbm, nbmcd, nbmp, nbnl, indic,&
                  nbf, impr, itrans, epst, icoupl,&
                  tpfl, veci1, locfl0, dt0, tfexm,&
                  ts, iarch, nexcit, tabexc, numexc,&
                  masgi, amori, pulsi, vecr3, phii,&
                  parcho, noecho, intitu, vecr5, vecr1,&
                  vecr2, vgap, vecr4, nbchoc, depg0,&
                  vitg0, xsi0, nbsauv)
! aslint: disable=W1504
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA REPONSE DYNAMIQUE NON-LINEAIRE D'UNE
! -----------   STRUCTURE PAR UNE METHODE INTEGRALE
!               CREATION DES OBJETS DE TRAVAIL - PREPARATION DES DONNEES
!
!               APPELANT : MDTR74
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! -------------------------
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/chveri.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mditm2.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbm, nbmcd, nbmp, nbnl, indic, nbf, impr, itrans
    real(kind=8) :: epst
    integer :: icoupl
    character(len=8) :: tpfl
    integer :: veci1(*)
    logical :: locfl0(*)
    real(kind=8) :: dt0, tfexm, ts
    integer :: iarch, nexcit
    character(len=8) :: tabexc(*)
    integer :: numexc(*)
    real(kind=8) :: masgi(*), amori(*), pulsi(*), vecr3(*), phii(nbnl, nbm, *)
    real(kind=8) :: parcho(nbnl, *)
    character(len=8) :: noecho(nbnl, *), intitu(*)
    real(kind=8) :: vecr5(*), vecr1(*), vecr2(*), vgap, vecr4(*), depg0(*)
    real(kind=8) :: vitg0(*), xsi0(*)
    integer :: nbsauv
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ic, idrayo, idthet, im, imode, j, kfext, ktext, kinti, kncho
    integer :: n2, nbseg, nbseg0, nitmax, np3, nbchoc
    integer :: jtran
    integer :: iamo00, ipuld, ipul00, ifmo0, ifmoa, idepg, ivitg, iaccg, iaccg0
    integer :: ifx, ifxs, itx, itxs, ifxtr, ifx0
    integer :: iom, iaa, ibb, izin, izitr, iza1, iza2, iza3, is0, isr0, iz0
    integer :: iza4, iza5
    integer :: iamo, iamo0, ipul, ipul0, ifmo00, ifmot, ifmo0t, ifexmo, ifnlmo
    integer :: ifmres, idepge, idepgc, idepgt, jdepgt, ivitge, ivitgc, ivitgt
    integer :: jvitgt, iaccgt
    integer :: ikmo, icmo, ikmo0, icmo0, ikmo00, icmo00, ikmoca, icmoca, icmofa
    integer :: jflu0, jfluc
    integer :: itr, ivg, ivd, ivg0, ivd0, ivvg, irr, iri, irr0, jix, jixf, ji1
    integer :: ji2, im1, im2, im6, iftmp, idd, iu, iw, ilocfc, iloc
    integer :: jc1, jc2, jce, jcb, jifn, jtypch, jns, jia, icho, iorig, ialp
    integer :: ibet, igam, ioldf, ih, irc, ithe
    integer :: jdep, jvit, jacc, jdep0, jvit0, jacc0
    integer :: jdt, ibid, irett, nbval
    real(kind=8) :: dttr, r8bid
    complex(kind=8) :: cbid
    character(len=8) :: resu, nomobj, vecgen, k8typ, kbid
    character(len=16) :: typres, nomcmd
    character(len=24) :: nomfon
    character(len=24) :: valk(2)
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL      CHVERI, GETRES, JEDEMA, JELIRA, JEMARQ, JEVEUO,
!    &              MDITM2, WKVECT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
    if ((nexcit.eq.0) .or. (nbf.eq.0)) call u2mess('F', 'ALGORITH5_49')
!
    n2 = nbm + 2
    dttr = 0.0d0
!
    call wkvect('&&MDITM1.TRANS', 'V V R8', 2*2*nbm, jtran)
!
    call wkvect('&&MDITM1.AMOR00', 'V V R8', nbm, iamo00)
    call wkvect('&&MDITM1.PULSD', 'V V R8', nbm, ipuld)
    call wkvect('&&MDITM1.PULS00', 'V V R8', nbm, ipul00)
    do 10 im = 1, nbm
        zr(iamo00+im-1) = amori(im)/masgi(im)
        zr(ipuld +im-1) = pulsi(im)
        zr(ipul00+im-1) = pulsi(im)
10  end do
!
    call wkvect('&&MDITM1.FMOD0', 'V V R8', nbm, ifmo0)
    call wkvect('&&MDITM1.FMODA', 'V V R8', nbm, ifmoa)
!
    call wkvect('&&MDITM1.DEPG', 'V V R8', nbm, idepg)
    call wkvect('&&MDITM1.VITG', 'V V R8', nbm, ivitg)
    call wkvect('&&MDITM1.ACCG', 'V V R8', nbm, iaccg)
    call wkvect('&&MDITM1.ACCG0', 'V V R8', nbm, iaccg0)
!
    call wkvect('&&MDITM1.FEXT', 'V V R8', nbf*nbm, ifx)
    call wkvect('&&MDITM1.FEXTTS', 'V V R8', nbf*nbm, ifxs)
    call wkvect('&&MDITM1.TEXT', 'V V R8', nbf, itx)
    call wkvect('&&MDITM1.TEXTTS', 'V V R8', nbf, itxs)
    call wkvect('&&MDITM1.FEXTTR', 'V V R8', nbm, ifxtr)
    call wkvect('&&MDITM1.FEXTT0', 'V V R8', nbm, ifx0)
    vecgen = tabexc(1)
    call jeveuo(vecgen//'           .VALE', 'L', ktext)
    do 20 i = 1, nbf
        zr(itx+i-1) = zr(ktext+i-1)
20  end do
    dttr = (zr(itx+1) - zr(itx)) * 1.0d-02
    do 30 i = 1, nexcit
        vecgen = tabexc(i)
        call jeveuo(vecgen//'           .VALE', 'L', kfext)
        imode = numexc(i)
        do 31 j = 1, nbf
            zr(ifx+(imode-1)*nbf+j-1) = zr(kfext+nbf+j-1)
31      continue
30  end do
!
    call wkvect('&&MDITM1.OMEGAF', 'V V R8', nbf, iom)
    call wkvect('&&MDITM1.AA', 'V V R8', nbf*nbm, iaa)
    call wkvect('&&MDITM1.BB', 'V V R8', nbf*nbm, ibb)
    call wkvect('&&MDITM1.ZIN', 'V V C', nbm, izin)
    call wkvect('&&MDITM1.ZITR', 'V V C', nbm, izitr)
    call wkvect('&&MDITM1.ZA1', 'V V C', nbm, iza1)
    call wkvect('&&MDITM1.ZA2', 'V V C', nbm, iza2)
    call wkvect('&&MDITM1.ZA3', 'V V C', nbm, iza3)
    call wkvect('&&MDITM1.S0', 'V V C', nbm, is0)
    call wkvect('&&MDITM1.SR0', 'V V C', nbm, isr0)
    call wkvect('&&MDITM1.Z0', 'V V C', nbm, iz0)
    call wkvect('&&MDITM1.ZA4', 'V V C', nbf*nbm, iza4)
    call wkvect('&&MDITM1.ZA5', 'V V C', nbf*nbm, iza5)
!
!-----------------------------------------------------------------------
!     EN L'ABSCENCE DE NON-LINEARITES DE CHOC :
!     -> SORTIE EN ERREUR FATALE EN L'ETAT ACTUEL
!        PREVOIR LE DEVELOPPEMENT D'UNE PROCEDURE APPROPRIEE
!-----------------------------------------------------------------------
!
    if (nbnl .eq. 0) then
!
!
        call u2mess('F', 'ALGORITH5_50')
!
!-----------------------------------------------------------------------
!     EN PRESENCE DE NON-LINEARITES DE CHOC :
!     -> CREATION D'OBJETS DE TRAVAIL COMPLEMENTAIRES
!     -> APPEL DE L'ALGORITHME ITMI
!-----------------------------------------------------------------------
!
    else
!
!
        nitmax = 150
        call wkvect('&&MDITM1.VECDT', 'V V R8', nitmax+1, jdt)
!
        call wkvect('&&MDITM1.AMOR', 'V V R8', nbm, iamo)
        call wkvect('&&MDITM1.AMOR0', 'V V R8', nbm, iamo0)
        call wkvect('&&MDITM1.PULS', 'V V R8', nbm, ipul)
        call wkvect('&&MDITM1.PULS0', 'V V R8', nbm, ipul0)
        do 40 im = 1, nbm
            zr(iamo +im-1) = amori(im)/masgi(im)
            zr(iamo0 +im-1) = amori(im)/masgi(im)
            zr(ipul +im-1) = pulsi(im)
            zr(ipul0 +im-1) = pulsi(im)
40      end do
!
        call wkvect('&&MDITM1.FMOD00', 'V V R8', nbm, ifmo00)
        call wkvect('&&MDITM1.FMODT', 'V V R8', nbm, ifmot)
        call wkvect('&&MDITM1.FMOD0T', 'V V R8', nbm, ifmo0t)
        call wkvect('&&MDITM1.FEXMOD', 'V V R8', nbm, ifexmo)
        call wkvect('&&MDITM1.FNLMOD', 'V V R8', nbm, ifnlmo)
        call wkvect('&&MDITM1.FMRES', 'V V R8', nbm, ifmres)
!
        call wkvect('&&MDITM1.DEPGE', 'V V R8', nbm, idepge)
        call wkvect('&&MDITM1.DEPGC', 'V V R8', nbm, idepgc)
        call wkvect('&&MDITM1.DEPGT', 'V V R8', nbm, idepgt)
        call wkvect('&&MDITM1.DEPG0T', 'V V R8', nbm, jdepgt)
        call wkvect('&&MDITM1.VITGE', 'V V R8', nbm, ivitge)
        call wkvect('&&MDITM1.VITGC', 'V V R8', nbm, ivitgc)
        call wkvect('&&MDITM1.VITGT', 'V V R8', nbm, ivitgt)
        call wkvect('&&MDITM1.VITG0T', 'V V R8', nbm, jvitgt)
        call wkvect('&&MDITM1.ACCGT', 'V V R8', nbm, iaccgt)
!
        call wkvect('&&MDITM1.KMOD', 'V V R8', nbm*nbm, ikmo)
        call wkvect('&&MDITM1.CMOD', 'V V R8', nbm*nbm, icmo)
        call wkvect('&&MDITM1.KMOD0', 'V V R8', nbm*nbm, ikmo0)
        call wkvect('&&MDITM1.CMOD0', 'V V R8', nbm*nbm, icmo0)
        call wkvect('&&MDITM1.KMOD00', 'V V R8', nbm*nbm, ikmo00)
        call wkvect('&&MDITM1.CMOD00', 'V V R8', nbm*nbm, icmo00)
        call wkvect('&&MDITM1.KMODCA', 'V V R8', nbm*nbm, ikmoca)
        call wkvect('&&MDITM1.CMODCA', 'V V R8', nbm*nbm, icmoca)
        call wkvect('&&MDITM1.CMODFA', 'V V R8', nbm*nbm, icmofa)
        call wkvect('&&MDITM1.AMFLU0', 'V V R8', nbm*nbm, jflu0)
        call wkvect('&&MDITM1.AMFLUC', 'V V R8', nbm*nbm, jfluc)
!
        call wkvect('&&MDITM1.TTR', 'V V R8', n2*nbm, itr)
        call wkvect('&&MDITM1.VG', 'V V R8', nbm*nbm, ivg)
        call wkvect('&&MDITM1.VD', 'V V R8', nbm*nbm, ivd)
        call wkvect('&&MDITM1.VG0', 'V V R8', nbm*nbm, ivg0)
        call wkvect('&&MDITM1.VD0', 'V V R8', nbm*nbm, ivd0)
        call wkvect('&&MDITM1.VVG', 'V V R8', nbm*nbm, ivvg)
        call wkvect('&&MDITM1.RR', 'V V R8', nbm, irr)
        call wkvect('&&MDITM1.RI', 'V V R8', nbm, iri)
        call wkvect('&&MDITM1.RR0', 'V V R8', nbm, irr0)
!
        call wkvect('&&MDITM1.INDX', 'V V I', nbm, jix)
        call wkvect('&&MDITM1.INDXF', 'V V I', nbm, jixf)
        call wkvect('&&MDITM1.INTGE1', 'V V I', nbm, ji1)
        call wkvect('&&MDITM1.INTGE2', 'V V I', nbm, ji2)
        call wkvect('&&MDITM1.MTMP1', 'V V R8', nbm*nbm, im1)
        call wkvect('&&MDITM1.MTMP2', 'V V R8', nbm*nbm, im2)
        call wkvect('&&MDITM1.MTMP6', 'V V R8', 3*nbm, im6)
        call wkvect('&&MDITM1.FTMP', 'V V R8', nbm, iftmp)
        call wkvect('&&MDITM1.VECDD', 'V V R8', nbm, idd)
        call wkvect('&&MDITM1.VECU', 'V V R8', nbm, iu)
        call wkvect('&&MDITM1.VECW', 'V V R8', nbm, iw)
        call wkvect('&&MDITM1.LOCFLC', 'V V L', nbm, ilocfc)
        call wkvect('&&MDITM1.LOC', 'V V L', nbm, iloc)
!
        call getres(resu, typres, nomcmd)
        call jeveuo(resu//'           .NCHO', 'E', kncho)
        call jeveuo(resu//'           .INTI', 'E', kinti)
        do 50 ic = 1, nbnl
            zk8(kinti+ic-1) = intitu(ic)
            zk8(kncho+ic-1) = noecho(ic,1)
50      end do
!
        call wkvect('&&MDITM1.TCONF1', 'V V R8', 4*nbnl, jc1)
        call wkvect('&&MDITM1.TCONF2', 'V V R8', 4*nbnl, jc2)
        call wkvect('&&MDITM1.TCONFE', 'V V R8', 4*nbnl, jce)
        call wkvect('&&MDITM1.ICONFB', 'V V I', nbnl, jcb)
        call wkvect('&&MDITM1.ITFORN', 'V V I', nbnl, jifn)
        call wkvect('&&MDITM1.TYPCH', 'V V I', nbnl, jtypch)
        call wkvect('&&MDITM1.NBSEG', 'V V I', nbnl, jns)
        call wkvect('&&MDITM1.OLDIA', 'V V I', nbnl, jia)
!
        call wkvect('&&MDITM1.CHOC', 'V V R8', 6*nbnl, icho)
        call wkvect('&&MDITM1.ORIG', 'V V R8', 6*nbnl, iorig)
        call wkvect('&&MDITM1.ALPHA', 'V V R8', 2*nbnl, ialp)
        call wkvect('&&MDITM1.BETA', 'V V R8', 2*nbnl, ibet)
        call wkvect('&&MDITM1.GAMMA', 'V V R8', 2*nbnl, igam)
        call wkvect('&&MDITM1.OLDF', 'V V R8', 9*nbnl, ioldf)
        call wkvect('&&MDITM1.NOCH', 'V V K8', nbnl, ih)
        do 60 ic = 1, nbnl
            zr(icho + 6*(ic-1) + 1 - 1) = parcho(ic,2)
            zr(icho + 6*(ic-1) + 2 - 1) = parcho(ic,3)
            zr(icho + 6*(ic-1) + 3 - 1) = parcho(ic,4)
            zr(icho + 6*(ic-1) + 4 - 1) = parcho(ic,5)
            zr(icho + 6*(ic-1) + 5 - 1) = parcho(ic,6)
            zr(icho + 6*(ic-1) + 6 - 1) = parcho(ic,7)
            zr(iorig + 6*(ic-1) + 1 - 1) = parcho(ic,14)
            zr(iorig + 6*(ic-1) + 2 - 1) = parcho(ic,15)
            zr(iorig + 6*(ic-1) + 3 - 1) = parcho(ic,16)
            zr(iorig + 6*(ic-1) + 4 - 1) = parcho(ic,8)
            zr(iorig + 6*(ic-1) + 5 - 1) = parcho(ic,9)
            zr(iorig + 6*(ic-1) + 6 - 1) = parcho(ic,10)
            zr(ialp + 2*(ic-1) + 1 - 1) = parcho(ic,17)
            zr(ialp + 2*(ic-1) + 2 - 1) = parcho(ic,18)
            zr(ibet + 2*(ic-1) + 1 - 1) = parcho(ic,19)
            zr(ibet + 2*(ic-1) + 2 - 1) = parcho(ic,20)
            zr(igam + 2*(ic-1) + 1 - 1) = parcho(ic,21)
            zr(igam + 2*(ic-1) + 2 - 1) = parcho(ic,22)
60      end do
!
        nbseg0 = 1
        do 70 ic = 1, nbnl
            zk8(ih+ic-1) = noecho(ic,1)
            if (noecho(ic,9) .eq. 'PLAN_Y  ') then
                zi(jtypch+ic-1) = 0
                zi(jns+ic-1) = 1
            else if (noecho(ic,9).eq.'PLAN_Z  ') then
                zi(jtypch+ic-1) = 1
                zi(jns+ic-1) = 1
            else if (noecho(ic,9).eq.'CERCLE  ') then
                zi(jtypch+ic-1) = 2
                zi(jns+ic-1) = 1
                else if ( noecho(ic,9).eq.'BI_CERCL' .or. noecho(ic,9)&
            .eq.'BI_PLANY' .or. noecho(ic,9).eq.'BI_PLANZ' ) then
                valk(1) = noecho(ic,9)
                valk(2) = noecho(ic,1)
                call u2mesk('F', 'ALGORITH5_51', 2, valk)
            else
                zi(jtypch+ic-1) = 3
                nomobj = noecho(ic,9)
                call tbliva(nomobj, 1, 'LIEU', ibid, r8bid,&
                            cbid, 'DEFIOBST', kbid, r8bid, 'FONCTION',&
                            k8typ, ibid, r8bid, cbid, nomfon,&
                            irett)
                ASSERT(irett.eq.0)
                call jelira(nomfon(1:19)//'.VALE', 'LONMAX', nbval)
                nbseg = nbval/2
                zi(jns+ic-1) = nbseg
                if (nbseg .gt. nbseg0) nbseg0 = nbseg
            endif
70      end do
        np3 = nbseg0
        call wkvect('&&MDITM1.RC', 'V V R8', np3*nbnl, irc)
        call wkvect('&&MDITM1.THETA', 'V V R8', np3*nbnl, ithe)
        do 80 ic = 1, nbnl
            if (zi(jtypch+ic-1) .eq. 3) then
                nomobj = noecho(ic,9)
                call tbliva(nomobj, 1, 'LIEU', ibid, r8bid,&
                            cbid, 'DEFIOBST', kbid, r8bid, 'FONCTION',&
                            k8typ, ibid, r8bid, cbid, nomfon,&
                            irett)
                ASSERT(irett.eq.0)
                call jeveuo(nomfon(1:19)//'.VALE', 'L', idthet)
                call jelira(nomfon(1:19)//'.VALE', 'LONMAX', nbval)
                nbseg = nbval/2
                idrayo = idthet + nbseg
                do 81 i = 1, nbseg
                    zr(irc+nbseg0*(ic-1)+i-1) = zr(idrayo + i - 1)
                    zr(ithe+nbseg0*(ic-1)+i-1) = zr(idthet + i - 1)
81              continue
            else
                zr(irc+nbseg0*(ic-1)) = parcho(ic,1)
            endif
80      end do
        call chveri(nbm, nbnl, np3, nbm, nbm,&
                    nbnl, zi(jtypch), zi(jns), phii, noecho,&
                    zr(ialp), zr(ibet), zr(igam), zr(iorig), zr(irc),&
                    zr( ithe), zr(idepg))
!
        call wkvect('&&MDITM1.DEP', 'V V R8', 3*nbnl, jdep)
        call wkvect('&&MDITM1.VIT', 'V V R8', 3*nbnl, jvit)
        call wkvect('&&MDITM1.ACC', 'V V R8', 3*nbnl, jacc)
        call wkvect('&&MDITM1.DEP0', 'V V R8', 3*nbnl, jdep0)
        call wkvect('&&MDITM1.VIT0', 'V V R8', 3*nbnl, jvit0)
        call wkvect('&&MDITM1.ACC0', 'V V R8', 3*nbnl, jacc0)
!
        call mditm2(nbnl, np3, nbf, n2, nbm,&
                    nbmcd, nbmp, nbnl, indic, impr,&
                    itrans, epst, icoupl, tpfl, veci1,&
                    locfl0, dt0, tfexm, ts, dttr,&
                    zr(jdt), iarch, vitg0, depg0, masgi,&
                    amori, pulsi, phii, vecr5, vecr3,&
                    vecr1, vecr2, vgap, vecr4, xsi0,&
                    nbsauv, zi(jix), zi(jixf), zi(ji1), zi(ji2),&
                    zi(jcb), zr(jc1), zr(jc2), zr(jce), zi(jtypch),&
                    zi(jns), zi(jia), zi(jifn), zr(iamo), zr(iamo0),&
                    zr(iamo00), zr(ipul), zr(ipul0), zr( ipul00), zr(jtran),&
                    zr(ipuld), zr(ifmo0), zr(ifmo00), zr(ifmot), zr(ifmo0t),&
                    zr(ifexmo), zr(ifnlmo), zr(ifmoa), zr(ifmres), zr( idepg),&
                    zr(idepge), zr(idepgc), zr(idepgt), zr(jdepgt), zr(ivitg),&
                    zr(ivitge), zr(ivitgc), zr(ivitgt), zr(jvitgt), zr(iaccg),&
                    zr( iaccg0), zr(iaccgt), zr(jdep), zr(jvit), zr(jacc),&
                    zr(jdep0), zr( jvit0), zr(jacc0), zr(ikmo), zr(icmo),&
                    zr(ikmo0), zr(icmo0), zr( ikmo00), zr(icmo00), zr(ikmoca),&
                    zr(icmoca), zr(icmofa), zr(jflu0), zr(jfluc), zr(ivg),&
                    zr(ivd), zr(itr), zr(ivg0), zr(ivd0), zr(ivvg),&
                    zr(irr), zr(iri), zr(irr0), zr(im1), zr(im2),&
                    zr(im6), zr(iftmp), zr(idd), zr(iu), zr(iw),&
                    zr(iom), zr(iaa), zr(ibb), zr(ifx), zr( ifxs),&
                    zr(itx), zr(itxs), zr(ifxtr), zr(ifx0), zk8(ih),&
                    zr(icho), zr(iorig), zr(irc), zr(ithe), zr(ialp),&
                    zr(ibet), zr(igam), zr( ioldf), zl(ilocfc), zl(iloc),&
                    zc(is0), zc(iz0), zc(isr0), zc(iza1), zc(iza2),&
                    zc(iza3), zc(iza4), zc(iza5), zc(izin), zc(izitr),&
                    nbchoc, parcho, noecho)
!
    endif
!
    call jedema()
!
! --- FIN DE MDITM1.
end subroutine
