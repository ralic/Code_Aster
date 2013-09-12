subroutine caliai(fonree, charge)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    character(len=4) :: fonree
    character(len=8) :: charge
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
!
!     TRAITER LE MOT CLE LIAISON_DDL DE AFFE_CHAR_XXX
!     ET ENRICHIR LA CHARGE (CHARGE) AVEC LES RELATIONS LINEAIRES
!
! IN       : FONREE : 'REEL' OU 'FONC' OU 'COMP'
! IN/JXVAR : CHARGE : NOM D'UNE SD CHARGE
! ----------------------------------------------------------------------
    integer :: vali(2)
!
    complex(kind=8) :: betac
    character(len=2) :: typlag
    character(len=4) :: typcoe, typval, typco2
    character(len=7) :: typcha
    character(len=8) :: betaf
    character(len=8) :: k8bid, motcle, mogrou, mod, noma, nomnoe, char
    character(len=16) :: motfac, concep, oper
    character(len=19) :: lisrel
    character(len=24) :: trav, grouno, noeuma
    character(len=24) :: valk(3)
    character(len=15) :: coordo
    character(len=1) :: nompar(3)
    real(kind=8) :: valpar(3), vale
    integer :: iarg
!-----------------------------------------------------------------------
    integer :: i, ibid, ier, igr, in, indnoe, ino
    integer :: iocc, iret, j, jcmuc, jcmuf, jcmur, jcoor
    integer :: jddl, jdime, jdirec, jgr0, jjj, jlist1, jlist2
    integer :: k, n, n1, n2, n3, nb, nbgt
    integer :: nbno, ndim1, ndim2, nent, ng, ngr, nliai
    integer :: nno
    real(kind=8) :: beta
!-----------------------------------------------------------------------
    data nompar /'X','Y','Z'/
! ----------------------------------------------------------------------
!
    call jemarq()
    motfac = 'LIAISON_DDL     '
    motcle = 'NOEUD'
    mogrou = 'GROUP_NO'
    typlag = '12'
    typco2='REEL'
!
    lisrel = '&&CALIAI.RLLISTE'
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 90
!
    betac = (1.0d0,0.0d0)
!
    call dismoi('F', 'TYPE_CHARGE', charge, 'CHARGE', ibid,&
                typcha, ier)
    call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                mod, ier)
    call dismoi('F', 'NOM_MAILLA', charge, 'CHARGE', ibid,&
                noma, ier)
!
    noeuma = noma//'.NOMNOE'
    grouno = noma//'.GROUPENO'
    coordo = noma//'.COORDO'
    call jeveuo(coordo//'    .VALE', 'L', jcoor)
!
!     -- CALCUL DE NDIM1 : NBRE DE TERMES MAXI D'UNE LISTE
!        DE GROUP_NO OU DE NOEUD
!        --------------------------------------------------
    ndim1 = 0
    do 10 i = 1, nliai
        call getvtx(motfac, mogrou, iocc=i, nbval=0, nbret=nent)
        ndim1 = max(ndim1,-nent)
        call getvtx(motfac, motcle, iocc=i, nbval=0, nbret=nent)
        ndim1 = max(ndim1,-nent)
10  end do
!
    trav = '&&CALIAI.'//motfac
    call wkvect(trav, 'V V K24', ndim1, jjj)
!
!
!     -- CALCUL DE NDIM2 ET VERIFICATION DES NOEUDS ET GROUP_NO
!        NDIM2 EST LE NOMBRE MAXI DE NOEUDS IMPLIQUES DANS UNE
!        RELATION LINEAIRE
!        -------------------------------------------------------
    ndim2 = ndim1
    do 40 iocc = 1, nliai
        call getvtx(motfac, mogrou, iocc=iocc, nbval=ndim1, vect=zk24(jjj),&
                    nbret=ngr)
        nbgt = 0
        do 20 igr = 1, ngr
            call jeexin(jexnom(grouno, zk24(jjj+igr-1)), iret)
            if (iret .eq. 0) then
                valk(1) = zk24(jjj+igr-1)
                valk(2) = noma
                call u2mesk('F', 'MODELISA2_95', 2, valk)
            else
                call jelira(jexnom(grouno, zk24(jjj+igr-1)), 'LONUTI', n1)
                nbgt = nbgt + n1
            endif
20      continue
        ndim2 = max(ndim2,nbgt)
        call getvtx(motfac, motcle, iocc=iocc, nbval=ndim1, vect=zk24(jjj),&
                    nbret=nno)
        do 30 ino = 1, nno
            call jenonu(jexnom(noeuma, zk24(jjj+ino-1)), iret)
            if (iret .eq. 0) then
                valk(1) = motcle
                valk(2) = zk24(jjj+ino-1)
                valk(3) = noma
                call u2mesk('F', 'MODELISA2_96', 3, valk)
            endif
30      continue
40  end do
!
!     -- ALLOCATION DE TABLEAUX DE TRAVAIL
!    -------------------------------------
    call wkvect('&&CALIAI.LISTE1', 'V V K24', ndim1, jlist1)
    call wkvect('&&CALIAI.LISTE2', 'V V K8', ndim2, jlist2)
    call wkvect('&&CALIAI.DDL  ', 'V V K8', ndim2, jddl)
    call wkvect('&&CALIAI.COEMUR', 'V V R', ndim2, jcmur)
    call wkvect('&&CALIAI.COEMUC', 'V V C', ndim2, jcmuc)
    call wkvect('&&CALIAI.COEMUF', 'V V K8', ndim2, jcmuf)
    call wkvect('&&CALIAI.DIRECT', 'V V R', 3*ndim2, jdirec)
    call wkvect('&&CALIAI.DIMENSION', 'V V I', ndim2, jdime)
!
!     BOUCLE SUR LES RELATIONS LINEAIRES
!     -----------------------------------
    call getres(char, concep, oper)
    do 80 i = 1, nliai
        call getvr8(motfac, 'COEF_MULT', iocc=i, nbval=ndim2, vect=zr(jcmur),&
                    nbret=n2)
        if (oper .eq. 'AFFE_CHAR_MECA_F') then
            call getvid(motfac, 'COEF_MULT_FONC', iocc=i, nbval=ndim2, vect=zk8( jcmuf),&
                        nbret=n3)
        else
            n3=0
        endif
        if (n3 .ne. 0) typco2='FONC'
        call getvtx(motfac, 'DDL', iocc=i, nbval=ndim2, vect=zk8(jddl),&
                    nbret=n1)
        typcoe = 'REEL'
!
!
!        EXCEPTION :SI LE MOT-CLE DDL N'EXISTE PAS DANS AFFE_CHAR_THER,
!        ON CONSIDERE QUE LES RELATIONS LINEAIRES PORTENT
!        SUR LE DDL 'TEMP'
        if (n1 .eq. 0 .and. typcha(1:4) .eq. 'THER') then
            n1 = ndim2
            do 50 k = 1, n1
                zk8(jddl-1+k) = 'TEMP'
50          continue
        endif
!
        if (n1 .ne. (n2+n3)) then
            vali (1) = abs(n1)
            vali (2) = abs(n2+n3)
            call u2mesg('F', 'MODELISA8_46', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
!
!
!       -- RECUPERATION DU 2ND MEMBRE :
!       ------------------------------
        if (fonree .eq. 'REEL') then
            call getvr8(motfac, 'COEF_IMPO', iocc=i, scal=beta, nbret=nb)
            typval = 'REEL'
        else if (fonree.eq.'FONC') then
            call getvid(motfac, 'COEF_IMPO', iocc=i, scal=betaf, nbret=nb)
            typval = 'FONC'
        else if (fonree.eq.'COMP') then
            call getvc8(motfac, 'COEF_IMPO', iocc=i, scal=betac, nbret=nb)
            typval = 'COMP'
        else
            call u2mess('F', 'DVP_1')
        endif
!
!
        call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO', i,&
                    iarg, 0, zk24(jlist1), ng)
        if (ng .ne. 0) then
!
!           -- CAS DE GROUP_NO :
!           --------------------
            ng = -ng
            call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO', i,&
                        iarg, ng, zk24(jlist1), n)
            indnoe = 0
            do 70 j = 1, ng
                call jeveuo(jexnom(grouno, zk24(jlist1-1+j)), 'L', jgr0)
                call jelira(jexnom(grouno, zk24(jlist1-1+j)), 'LONUTI', n)
                do 60 k = 1, n
                    in = zi(jgr0-1+k)
                    indnoe = indnoe + 1
                    call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
                    zk8(jlist2+indnoe-1) = nomnoe
                    if (typco2 .eq. 'FONC') then
                        valpar(1) = zr(jcoor-1+3*(in-1)+1)
                        valpar(2) = zr(jcoor-1+3*(in-1)+2)
                        valpar(3) = zr(jcoor-1+3*(in-1)+3)
                        call fointe('F', zk8(jcmuf-1+indnoe), 3, nompar, valpar,&
                                    vale, ier)
                        zr(jcmur-1+indnoe)=vale
                    endif
60              continue
70          continue
!
!           -- ON VERIFIE QUE LE NOMBRE DE NOEUDS DES GROUP_NO
!              EST EGAL AU NOMBRE DE DDLS DE LA RELATION :
!              -----------------------------------------
            if (n1 .ne. indnoe) then
                vali (1) = abs(n1)
                vali (2) = indnoe
                call u2mesg('F', 'MODELISA8_47', 0, ' ', 2,&
                            vali, 0, 0.d0)
            endif
!
!           AFFECTATION A LA LISTE DE RELATIONS
!
            call afrela(zr(jcmur), zc(jcmuc), zk8(jddl), zk8(jlist2), zi(jdime),&
                        zr(jdirec), indnoe, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
!
        else
!
!           CAS DE NOEUD :
!           -------------
            call getvem(noma, 'NOEUD', motfac, 'NOEUD', i,&
                        iarg, 0, zk8( jlist2), nbno)
            if (nbno .ne. 0) then
                nbno = -nbno
                call getvem(noma, 'NOEUD', motfac, 'NOEUD', i,&
                            iarg, nbno, zk8(jlist2), n)
                if (typco2 .eq. 'FONC') then
                    do 100 k = 1, n
                        call jenonu(jexnom(noma//'.NOMNOE', zk8(jlist2- 1+k)), in)
                        valpar(1) = zr(jcoor-1+3*(in-1)+1)
                        valpar(2) = zr(jcoor-1+3*(in-1)+2)
                        valpar(3) = zr(jcoor-1+3*(in-1)+3)
                        call fointe('F', zk8(jcmuf-1+k), 3, nompar, valpar,&
                                    vale, ier)
                        zr(jcmur-1+k)=vale
100                  continue
                endif
            endif
!
!           -- ON VERIFIE QUE LE NOMBRE DE NOEUDS DE LA LISTE DE
!              NOEUDS EST EGAL AU NOMBRE DE DDLS DE LA RELATION :
!              ------------------------------------------------
            if (n1 .ne. nbno) then
                vali (1) = abs(n1)
                vali (2) = nbno
                call u2mesg('F', 'MODELISA8_47', 0, ' ', 2,&
                            vali, 0, 0.d0)
            endif
            call afrela(zr(jcmur), zc(jcmuc), zk8(jddl), zk8(jlist2), zi(jdime),&
                        zr(jdirec), nbno, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
        endif
!
80  end do
!
!     -- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ---------------------------------------------
    call aflrch(lisrel, charge)
!
!     -- MENAGE :
!     -----------
    call jedetr(trav)
    call jedetr('&&CALIAI.LISTE1')
    call jedetr('&&CALIAI.LISTE2')
    call jedetr('&&CALIAI.DDL  ')
    call jedetr('&&CALIAI.COEMUR')
    call jedetr('&&CALIAI.COEMUC')
    call jedetr('&&CALIAI.COEMUF')
    call jedetr('&&CALIAI.DIRECT')
    call jedetr('&&CALIAI.DIMENSION')
!
90  continue
    call jedema()
end subroutine
