subroutine calyrc(chargz)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/calyrg.h"
#include "asterfort/canort.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/imprel.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbnlma.h"
#include "asterfort/orilma.h"
#include "asterfort/pj2dco.h"
#include "asterfort/pj3dco.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chargz
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
!
!
!     CREER LES CARTES CHAR.CHXX.CMULT ET CHAR.CHXX.CIMPO
!          ET REMPLIR LIGRCH, POUR LE MOT-CLE LIAISON_CYCL
!     (COMMANDES AFFE_CHAR_MECA ET AFFE_CHAR_THER)
!
! IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
!-----------------------------------------------------------------------
!
    integer :: k, kk, nuno1, nuno2, ino1, ino2, ndim, ier, nocc, iocc
    integer :: ibid, jnoma, nnomx, idmax, idnomn, idcoef, jcmuc, idnomd
    integer :: idirec, idimen, iagma1, iagma2, nbma1, nbma2
    integer :: nbno2, idcal1, idcal2, nul
    integer :: iconb1, iconu1, icocf1, iconb2, iconu2, icocf2
    integer :: nno11, nno12, i, indire, lno
    integer :: nbtyp, nddl2, jlistk, jdim, ndim1
    integer :: jnunoe, jnorm, idim, ij, norien, ntrait
    integer :: icoef1, icoef2, icoef3, iagno3, nbno3, nbma3, idmai3
    logical :: lrota, dnor, lreori
    real(kind=8) :: beta, coef1, mrota(3, 3), zero, normal(3)
    real(kind=8) :: r8b
    real(kind=8) :: coef11, coef12, coef3
    complex(kind=8) :: betac
    character(len=1) :: kb
    character(len=2) :: typlag
    character(len=4) :: fonree
    character(len=4) :: typcoe, typlia
    character(len=8) :: k8b, noma, mo, m8blan
    character(len=8) :: kbeta, nono1, nono2, charge, cmp, ddl2, listyp(10)
    character(len=16) :: motfac, cores1, cores2, tymocl(4), motcle(4), nomcmd
    character(len=19) :: ligrmo
    character(len=19) :: lisrel
    character(len=24) :: geom3
    character(len=24) :: valk(2)
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
    nul=0
    motfac = 'LIAISON_CYCL'
    call getfac(motfac, nocc)
    if (nocc .eq. 0) goto 310
!
    call getres(kb, kb, nomcmd)
    if (nomcmd .eq. 'AFFE_CHAR_MECA') then
        typlia = 'DEPL'
    else if (nomcmd.eq.'AFFE_CHAR_THER') then
        typlia = 'TEMP'
    else
        ASSERT(.false.)
    endif
!
!
    fonree = 'REEL'
    typcoe = 'REEL'
    charge = chargz
!
    lisrel = '&&CALYRC.RLLISTE'
    zero = 0.0d0
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    kbeta = ' '
    typlag = '12'
    m8blan = '        '
    ndim1 = 3
    lreori = .false.
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mo, ier)
    ligrmo = mo//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
    call dismoi('F', 'DIM_GEOM', mo, 'MODELE', ndim,&
                k8b, ier)
    if (.not.(ndim.eq.2.or.ndim.eq.3)) call u2mess('F', 'MODELISA2_6')
!
    if (ndim .eq. 2) then
        nbtyp = 3
        listyp(1) = 'SEG2'
        listyp(2) = 'SEG3'
        listyp(3) = 'SEG4'
    else if (ndim.eq.3) then
        nbtyp = 10
        listyp(1) = 'TRIA3'
        listyp(2) = 'TRIA6'
        listyp(3) = 'TRIA9'
        listyp(4) = 'QUAD4'
        listyp(5) = 'QUAD8'
        listyp(6) = 'QUAD9'
        listyp(7) = 'QUAD12'
        listyp(8) = 'SEG2'
        listyp(9) = 'SEG3'
        listyp(10) = 'SEG4'
    endif
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nnomx,&
                kb, ier)
    idmax = nnomx + 3
    call wkvect('&&CALYRC.NOMNOE', 'V V K8', idmax, idnomn)
    call wkvect('&&CALYRC.NOMDDL', 'V V K8', idmax, idnomd)
    call wkvect('&&CALYRC.COEF', 'V V R', idmax, idcoef)
    call wkvect('&&CALYRC.COEMUC', 'V V C', 1, jcmuc)
    call wkvect('&&CALYRC.DIRECT', 'V V R', idmax*3, idirec)
    call wkvect('&&CALYRC.DIMENSION', 'V V I', idmax, idimen)
!
    cores1 = '&&CALYRC.CORES1'
    cores2 = '&&CALYRC.CORES2'
!
    do iocc = 1, nocc
!
        dnor = .false.
        if (typlia .eq. 'DEPL') then
            call getvtx(motfac, 'DDL_ESCL', iocc, iarg, 1,&
                        ddl2, nddl2)
            if (nddl2 .gt. 0) dnor = .true.
        endif
!
!        1.1 RECUPERATION DE LA LISTE DES MAILLE_MAIT :
!        ----------------------------------------------
!        -- 1er groupe maitre --
        motcle(1) = 'MAILLE_MAIT1'
        tymocl(1) = 'MAILLE'
        motcle(2) = 'GROUP_MA_MAIT1'
        tymocl(2) = 'GROUP_MA'
        call reliem(mo, noma, 'NU_MAILLE', motfac, iocc,&
                    2, motcle, tymocl, '&&CALYRC.LIMANU1', nbma1)
        call jeveuo('&&CALYRC.LIMANU1', 'L', iagma1)
!        -- 2eme groupe maitre --
        motcle(1) = 'MAILLE_MAIT2'
        tymocl(1) = 'MAILLE'
        motcle(2) = 'GROUP_MA_MAIT2'
        tymocl(2) = 'GROUP_MA'
        call reliem(mo, noma, 'NU_MAILLE', motfac, iocc,&
                    2, motcle, tymocl, '&&CALYRC.LIMANU2', nbma2)
        if (nbma2 .gt. 0) then
            call jeveuo('&&CALYRC.LIMANU2', 'L', iagma2)
        endif
!
!        1.2 RECUPERATION DES NOEUD_ESCL
!        -------------------------------
        if (.not.dnor) then
!
!        -- RECUPERATION DE LA LISTE DES NOEUD_ESCL :
!        --------------------------------------------
            motcle(1) = 'NOEUD_ESCL'
            tymocl(1) = 'NOEUD'
            motcle(2) = 'GROUP_NO_ESCL'
            tymocl(2) = 'GROUP_NO'
            motcle(3) = 'MAILLE_ESCL'
            tymocl(3) = 'MAILLE'
            motcle(4) = 'GROUP_MA_ESCL'
            tymocl(4) = 'GROUP_MA'
            call reliem(' ', noma, 'NU_NOEUD', motfac, iocc,&
                        4, motcle, tymocl, '&&CALYRC.LINONU', nbno3)
            call jeveuo('&&CALYRC.LINONU', 'L', iagno3)
!
        else
!
!        -- RECUPERATION DE LA LISTE DES MAILLE_ESCL :
!        ---------------------------------------------
            motcle(1) = 'MAILLE_ESCL'
            tymocl(1) = 'MAILLE'
            motcle(2) = 'GROUP_MA_ESCL'
            tymocl(2) = 'GROUP_MA'
            call reliem(mo, noma, 'NU_MAILLE', motfac, iocc,&
                        2, motcle, tymocl, '&&CALYRC.LIMANU3', nbma3)
            if (nbma3 .eq. 0) then
                valk(1) = motcle(1)
                valk(2) = motcle(2)
                call u2mesg('F', 'MODELISA8_49', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jeveuo('&&CALYRC.LIMANU3', 'L', idmai3)
!
            norien = 0
            call orilma(noma, ndim, zi(idmai3), nbma3, norien,&
                        ntrait, lreori, nul, ibid)
            if (norien .ne. 0) then
                call u2mess('F', 'MODELISA3_19')
            endif
!
! ---        CREATION DU TABLEAU DES NUMEROS DES NOEUDS '&&NBNLMA.LN'
! ---        ET DES NOMBRES D'OCCURENCES DE CES NOEUDS '&&NBNLMA.NBN'
! ---        DES MAILLES DE PEAU MAILLE_ESCL :
!            -------------------------------
            call nbnlma(noma, nbma3, zi(idmai3), nbtyp, listyp,&
                        nbno3)
!
! ---        CALCUL DES NORMALES EN CHAQUE NOEUD :
!            -----------------------------------
            call wkvect('&&CALYRC.LISTK', 'V V K8', 1, jlistk)
            call jeveuo('&&NBNLMA.LN', 'L', jnunoe)
!
! ---        CREATION DU TABLEAU D'INDIRECTION ENTRE LES INDICES
! ---        DU TABLEAU DES NORMALES ET LES NUMEROS DES NOEUDS :
!            -------------------------------------------------
            call wkvect('&&CALYRC.INDIRE', 'V V I', nnomx, indire)
            call jelira('&&NBNLMA.LN', 'LONUTI', lno, kb)
!
            do 20 i = 1, lno
                zi(indire+zi(jnunoe+i-1)-1) = i
20          continue
!
            call canort(noma, nbma3, zi(idmai3), ndim,&
                        nbno3, zi(jnunoe), 1)
            call jeveuo('&&CANORT.NORMALE', 'L', jnorm)
            call jedupo('&&NBNLMA.LN3', 'V', '&&CALYRC.LINONU', .false.)
            call jeveuo('&&CALYRC.LINONU', 'L', iagno3)
        endif
!
!
!       1.3 TRANSFORMATION DE LA GEOMETRIE DE GRNO2 :
!       ------------------------------------------
        geom3 = '&&CALYRC.GEOM_TRANSF'
        call calyrg(iocc, ndim, noma, '&&CALYRC.LINONU', geom3,&
                    mrota, lrota)
!
!       2. CALCUL DE CORRES :
!       -------------------
        if (ndim .eq. 2) then
!        -- 1er groupe esclave / 1er groupe maitre --
            call pj2dco('PARTIE', mo, mo, nbma1, zi(iagma1),&
                        nbno3, zi( iagno3), ' ', geom3, cores1,&
                        .false., r8b)
            if (nbma2 .gt. 0) then
!        -- 1er groupe esclave  / 2eme groupe maitre --
                call pj2dco('PARTIE', mo, mo, nbma2, zi(iagma2),&
                            nbno3, zi( iagno3), ' ', geom3, cores2,&
                            .false., r8b)
            endif
        else if (ndim.eq.3) then
!        -- 1er groupe esclave / 1er groupe maitre --
            call pj3dco('PARTIE', mo, mo, nbma1, zi(iagma1),&
                        nbno3, zi( iagno3), ' ', geom3, cores1,&
                        .false., r8b)
            if (nbma2 .gt. 0) then
!        -- 1er groupe esclave  / 2eme groupe maitre --
                call pj3dco('PARTIE', mo, mo, nbma2, zi(iagma2),&
                            nbno3, zi( iagno3), ' ', geom3, cores2,&
                            .false., r8b)
            endif
        endif
!
!        -- 1er groupe maitre --
        call jeveuo(cores1//'.PJEF_NB', 'L', iconb1)
        call jeveuo(cores1//'.PJEF_NU', 'L', iconu1)
        call jeveuo(cores1//'.PJEF_CF', 'L', icocf1)
        call jelira(cores1//'.PJEF_NB', 'LONMAX', nbno2, kb)
!
        if (nbma2 .gt. 0) then
!        -- 2eme groupe maitre --
            call jeveuo(cores2//'.PJEF_NB', 'L', iconb2)
            call jeveuo(cores2//'.PJEF_NU', 'L', iconu2)
            call jeveuo(cores2//'.PJEF_CF', 'L', icocf2)
!         CALL JELIRA(CORES2//'.PJEF_NB','LONMAX',NBNO2,KB)
        endif
!
!
!       3. ECRITURE DES RELATIONS LINEAIRES :
!       =====================================
!
!       3.0 RECUPERATION D'UN FACTEUR :
!       =================
!        -- 1er groupe maitre --
        call getvr8(motfac, 'COEF_MAIT1', iocc, iarg, 1,&
                    coef11, icoef1)
        if (icoef1 .le. 0) then
            coef11 = 1.d0
        endif
        if (nbma2 .gt. 0) then
!        -- 2eme groupe maitre --
            call getvr8(motfac, 'COEF_MAIT2', iocc, iarg, 1,&
                        coef12, icoef2)
            if (icoef2 .le. 0) then
                coef12 = 1.d0
            endif
        endif
!        -- 1er groupe esclave --
        call getvr8(motfac, 'COEF_ESCL', iocc, iarg, 1,&
                    coef3, icoef3)
        if (icoef3 .le. 0) then
            coef3 = 1.d0
        endif
!
!       3.1 CAS "DEPL" :
!       =================
        if (typlia .eq. 'DEPL') then
!
!       -- 3.1.1 S'IL N'Y A PAS DE ROTATION :
!       -------------------------------------
            if (.not.lrota) then
                idcal1 = 0
                idcal2 = 0
                do 90 ino2 = 1, nbno3
!           NNO11: NB DE NOEUD_MAIT LIES A INO2 SELON CORES1
                    nno11 = zi(iconb1-1+ino2)
!           NNO12: NB DE NOEUD_MAIT LIES A INO2 SELON CORES2
                    if (nbma2 .gt. 0) then
                        nno12 = zi(iconb2-1+ino2)
                    else
                        nno12 = 0
                    endif
                    if ((nno11.eq.0) .and. (nno12.eq.0)) goto 90
!
                    nuno2 = ino2
                    call jenuno(jexnum(noma//'.NOMNOE', nuno2), nono2)
!
                    zk8(idnomn-1+1) = nono2
                    zr(idcoef-1+1) = -1.d0*coef3
!
                    do 30,ino1 = 1,nno11
                    nuno1 = zi(iconu1+idcal1-1+ino1)
                    coef1 = zr(icocf1+idcal1-1+ino1)
                    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                    zk8(idnomn+ino1) = nono1
                    zr(idcoef+ino1) = coef1*coef11
30                  continue
                    do 40,ino1 = 1,nno12
                    nuno1 = zi(iconu2+idcal2-1+ino1)
                    coef1 = zr(icocf2+idcal2-1+ino1)
                    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                    zk8(idnomn+nno11+ino1) = nono1
                    zr(idcoef+nno11+ino1) = coef1*coef12
40                  continue
!
!           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
!           -----------------------------------------------------
                    if (dnor) then
                        do 60 ino1 = 1, nno11 + nno12 + 1
                            zi(idimen+ino1-1) = ndim
                            zk8(idnomd-1+ino1) = 'DEPL'
                            do 50 idim = 1, ndim
                                zr(idirec+ (ino1-1)*ndim1+idim-1) =&
                                zr(jnorm+ (zi(indire+ino2-1)-1)*ndim+&
                                idim-1)
50                          continue
60                      continue
                        call afrela(zr(idcoef), zc(jcmuc), zk8(idnomd), zk8(idnomn), zi(idimen),&
                                    zr(idirec), nno11+ nno12+1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                    else
                        do 80,k = 1,ndim
                        if (k .eq. 1) cmp = 'DX'
                        if (k .eq. 2) cmp = 'DY'
                        if (k .eq. 3) cmp = 'DZ'
                        do 70,ino1 = 1,nno11 + nno12 + 1
                        zk8(idnomd-1+ino1) = cmp
70                      continue
                        call afrela(zr(idcoef), zc(jcmuc), zk8( idnomd), zk8(idnomn), zi(idimen),&
                                    zr(idirec), nno11+nno12+1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                        call imprel(motfac, nno11+nno12+1, zr( idcoef), zk8(idnomd), zk8(idnomn),&
                                    beta)
80                      continue
                    endif
                    idcal1 = idcal1 + nno11
                    idcal2 = idcal2 + nno12
90              continue
!
!       -- 3.1.2  S'IL Y A UNE ROTATION :
!       ---------------------------------
            else
                idcal1 = 0
                idcal2 = 0
!
                do 250 ino2 = 1, nbno2
!
! ---       NNO1: NB DE NOEUD_MAIT LIES A INO2 :
!           ------------------------------------
                    nno11 = zi(iconb1-1+ino2)
                    if (nbma2 .gt. 0) then
                        nno12 = zi(iconb2-1+ino2)
                    else
                        nno12 = 0
                    endif
                    if ((nno11.eq.0) .and. (nno12.eq.0)) goto 250
                    do 110 k = 1, idmax
                        zk8(idnomn+k-1) = m8blan
                        zk8(idnomd+k-1) = m8blan
                        zr(idcoef+k-1) = zero
                        zi(idimen+k-1) = 0
                        do 100 kk = 1, 3
                            zr(idirec+3* (k-1)+kk-1) = zero
100                      continue
110                  continue
!
                    normal(1) = zero
                    normal(2) = zero
                    normal(3) = zero
!
                    nuno2 = ino2
                    call jenuno(jexnum(noma//'.NOMNOE', nuno2), nono2)
!
                    if (dnor) then
                        ij = 1
                    else
                        ij = ndim
                    endif
!
                    do 120,ino1 = 1,nno11
                    nuno1 = zi(iconu1+idcal1-1+ino1)
                    coef1 = zr(icocf1+idcal1-1+ino1)
                    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                    zk8(idnomn+ij+ino1-1) = nono1
                    zr(idcoef+ij+ino1-1) = coef1*coef11
120                  continue
                    do 130,ino1 = 1,nno12
                    nuno1 = zi(iconu2+idcal2-1+ino1)
                    coef1 = zr(icocf2+idcal2-1+ino1)
                    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                    zk8(idnomn+nno11+ij+ino1-1) = nono1
                    zr(idcoef+nno11+ij+ino1-1) = coef1*coef12
130                  continue
!
!
!           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
!           -----------------------------------------------------
                    if (dnor) then
                        do 150 idim = 1, ndim
                            do 140 jdim = 1, ndim
                                normal(idim) = normal(idim) + mrota( jdim,idim)*zr(jnorm+ (zi(ind&
                                               &ire+ino2- 1)-1)*ndim+jdim-1)
140                          continue
150                      continue
                        zr(idcoef+1-1) = 1.0d0*coef3
                        zk8(idnomn+1-1) = nono2
                        zk8(idnomd+1-1) = 'DEPL'
                        zi(idimen+1-1) = ndim
                        do 160 idim = 1, ndim
                            zr(idirec+idim-1) = zr( jnorm+ (zi(indire+ ino2-1)-1 )*ndim+idim-1 )
160                      continue
                        do 180 ino1 = 2, nno11 + 1
                            zi(idimen+ino1-1) = ndim
                            zk8(idnomd-1+ino1) = 'DEPL'
                            do 170 idim = 1, ndim
                                zr(idirec+ (ino1-1)*ndim1+idim-1) = - normal( idim)
170                          continue
180                      continue
                        do 200 ino1 = 2, nno12 + 1
                            zi(idimen+nno11+ino1-1) = ndim
                            zk8(idnomd-1+nno11+ino1) = 'DEPL'
                            do 190 idim = 1, ndim
                                zr(idirec+nno11+ (ino1-1)*ndim1+idim-&
                                1) = -normal(idim)
190                          continue
200                      continue
                        call afrela(zr(idcoef), zc(jcmuc), zk8(idnomd), zk8(idnomn), zi(idimen),&
                                    zr(idirec), nno11+ nno12+1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                    else
                        do 240,k = 1,ndim
                        if (k .eq. 1) cmp = 'DX'
                        if (k .eq. 2) cmp = 'DY'
                        if (k .eq. 3) cmp = 'DZ'
                        do 210,ino1 = 1,nno11
                        zk8(idnomd+ndim+ino1-1) = cmp
210                      continue
                        do 220,ino1 = 1,nno12
                        zk8(idnomd+nno11+ndim+ino1-1) = cmp
220                      continue
                        do 230 kk = 1, ndim
                            if (kk .eq. 1) cmp = 'DX'
                            if (kk .eq. 2) cmp = 'DY'
                            if (kk .eq. 3) cmp = 'DZ'
                            zk8(idnomn+kk-1) = nono2
                            zk8(idnomd+kk-1) = cmp
                            zr(idcoef+kk-1) = -mrota(kk,k)*coef3
230                      continue
                        call afrela(zr(idcoef), zc(jcmuc), zk8( idnomd), zk8(idnomn), zi(idimen),&
                                    zr(idirec), nno11+nno12+ndim, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                        call imprel(motfac, nno11+nno12+ndim, zr( idcoef), zk8(idnomd),&
                                    zk8(idnomn), beta)
240                      continue
                    endif
                    idcal1 = idcal1 + nno11
                    idcal2 = idcal2 + nno12
250              continue
            endif
!
!
!       3.2 CAS "TEMP" :
!       =================
        else if (typlia.eq.'TEMP') then
            idcal1 = 0
            idcal2 = 0
            do 290 ino2 = 1, nbno2
!           NNO1: NB DE NOEUD_MAIT LIES A INO2
                nno11 = zi(iconb1-1+ino2)
                nno12 = zi(iconb2-1+ino2)
                if ((nno11.eq.0) .and. (nno12.eq.0)) goto 290
!
                nuno2 = ino2
                call jenuno(jexnum(noma//'.NOMNOE', nuno2), nono2)
!
                zk8(idnomn-1+1) = nono2
                zr(idcoef-1+1) = -1.d0*coef3
!
                do 260,ino1 = 1,nno11
                nuno1 = zi(iconu1+idcal1-1+ino1)
                coef1 = zr(icocf1+idcal1-1+ino1)
                call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                zk8(idnomn+ino1) = nono1
                zr(idcoef+ino1) = coef1*coef11
260              continue
                do 270,ino1 = 1,nno12
                nuno1 = zi(iconu2+idcal2-1+ino1)
                coef1 = zr(icocf2+idcal2-1+ino1)
                call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                zk8(idnomn+nno11+ino1) = nono1
                zr(idcoef+nno11+ino1) = coef1*coef12
270              continue
!
!           -- AFFECTATION DE LA RELATION CONCERNANT LE NOEUD INO2 :
!           -----------------------------------------------------
                cmp = 'TEMP'
                do 280,ino1 = 1,nno11 + nno12 + 1
                zk8(idnomd-1+ino1) = cmp
280              continue
                call afrela(zr(idcoef), zc(jcmuc), zk8(idnomd), zk8( idnomn), zi(idimen),&
                            zr(idirec), nno11+nno12+1, beta, betac, kbeta,&
                            typcoe, fonree, typlag, 1.d-6, lisrel)
                call imprel(motfac, nno11+nno12+1, zr(idcoef), zk8( idnomd), zk8(idnomn),&
                            beta)
                idcal1 = idcal1 + nno11
                idcal2 = idcal2 + nno12
290          continue
        else
            ASSERT(.false.)
        endif
!
        call detrsd('CORRESP_2_MAILLA', cores1)
        call detrsd('CORRESP_2_MAILLA', cores2)
        call jedetr(geom3)
        call jedetr('&&CALYRC.LIMANU1')
        call jedetr('&&CALYRC.LIMANU2')
        call jedetr('&&CALYRC.LIMANU3')
        call jedetr('&&CALYRC.LINONU')
        call jedetr('&&CALYRC.LISTK')
        call jedetr('&&CALYRC.INDIRE')
        call jedetr('&&NBNLMA.LN')
        call jedetr('&&NBNLMA.NBN')
        call jedetr('&&CANORT.NORMALE')
!
    end do
!
    call jedetr('&&CALYRC.COEMUC')
    call jedetr('&&CALYRC.NOMNOE')
    call jedetr('&&CALYRC.NOMDDL')
    call jedetr('&&CALYRC.COEF')
    call jedetr('&&CALYRC.DIRECT')
    call jedetr('&&CALYRC.DIMENSION')
!
! --- AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE :
!     ------------------------------------------------
    call aflrch(lisrel, charge)
!
310  continue
    call jedema()
end subroutine
