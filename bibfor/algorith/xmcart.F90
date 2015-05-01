subroutine xmcart(noma, defico, modele, resoco)
!
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
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/nocart.h"
#include "asterfort/xmimp3.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/xcalc_code.h"
!
    character(len=8) :: noma, modele
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM-GG
!
! CREATION DE LA CARTE CONTENANT LES INFOS DE CONTACT
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  MODELE   : NOM DU MODELE
!
! CONTENU DE LA CARTE
!
! 1  XPC    : COORDONNEE PARAMETRIQUE X DU POINT DE CONTACT
! 2  XPR    : COORDONNEE PARAMETRIQUE X DU PROJETE DU POINT DE CONTACT
! 3  YPR    : COORDONNEE PARAMETRIQUE Y DU PROJETE DU POINT DE CONTACT
! 4  TAU1(1): COMPOSANTE 1 DU VECTEUR TANGENT 1
! 5  TAU1(2): COMPOSANTE 2 DU VECTEUR TANGENT 1
! 6  TAU1(3): COMPOSANTE 3 DU VECTEUR TANGENT 1
! 7  TAU2(1): COMPOSANTE 1 DU VECTEUR TANGENT 2
! 8  TAU2(2): COMPOSANTE 2 DU VECTEUR TANGENT 2
! 9  TAU2(3): COMPOSANTE 3 DU VECTEUR TANGENT 2
! 10 YPC    : COORDONNEE PARAMETRIQUE Y DU POINT DE CONTACT
! 11 INDCO  : ETAT DE CONTACT (0 PAS DE CONTACT)
! 13 COEFCA : COEF_REGU_CONT
! 14 COEFFA : COEF_REGU_FROT
! 15 COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! 16 IFROTT : FROTTEMENT (0 SI PAS, 3 SI COULOMB)
! 17 INDNOR : NOEUD EXCLU PAR PROJECTION HORS ZONE
! 18 NINTER : NOMBRE DE POINT D'INTERSECTION DE LA MAILLE ESCLAVE
! 19 HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! 20 IGLISS : CONTACT GLISSIERE
! 21 MEMCO  : MEMOIRE DE CONTACT
! 22 NFAES  : NUMEROS DE LA FACETTES ESCLAVE
! 23 NFAMA  : NUMEROS DE LA FACETTES MAITRE
!
!
!
!
    integer :: nbch
    parameter (nbch=8)
!
    integer :: ncmp(nbch)
!
    integer :: nummae, nummam, nnoe, nnom, ifise, ifism, jfiss
    integer :: i, j, ipc, k, ntpc, ndim, izone, nface, npte
    integer :: ztabf, jtabf, jnosdc, nfhe, nfhm, ncmpe, ncmpm
    character(len=24) :: tabfin, nosdco
    integer :: jvalv(nbch), jncmp(nbch), jcesl(nbch), jcesd(nbch), jcesv(nbch), iad
    character(len=3) :: ch3
    character(len=8) :: nomgd
    character(len=19) :: ligrxf, chs(nbch), carte(nbch)
    integer :: zxain, ifm, niv, jconx, ninter, nbpi, ier
    aster_logical :: lmulti
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CREATION DE LA CARTE POUR LES'//&
        ' ELEMENTS DE CONTACT X-FEM'
    endif
!
! --- INITIALISATIONS
!
    ndim = cfdisi(defico,'NDIM')
    ntpc = cfdisi(defico,'NTPC')
    ncmp(1) = 34
    if (ndim .eq. 2) then
        ncmp(2) = 32
        ncmp(3) = 14
        ncmp(4) = 35
        ncmp(5) = 9
        ncmp(6) = 2
        ncmp(7) = 4
        ncmp(8) = 40
    else if (ndim.eq.3) then
        ncmp(2) = 64
        ncmp(3) = 102
        ncmp(4) = 170
        ncmp(5) = 90
        ncmp(6) = 2
        ncmp(7) = 8
        ncmp(8) = 80
    endif
!
    ztabf = cfmmvd('ZTABF')
    zxain = xxmmvd('ZXAIN')
!
! --- ACCES OBJETS
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    nosdco = resoco(1:14)//'.NOSDCO'
    call jeveuo(nosdco, 'L', jnosdc)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx)
!
! --- CHAMPS DES ELEMENTS XFEM
!
    chs(1) = '&&XMCART.CHS1'
    chs(2) = '&&XMCART.CHS2'
    chs(3) = '&&XMCART.CHS3'
    chs(4) = '&&XMCART.CHS4'
    chs(5) = '&&XMCART.CHS5'
    chs(6) = '&&XMCART.CHS6'
    chs(7) = '&&XMCART.CHS7'
    chs(8) = '&&XMCART.CHS8'
!
    call celces(modele//'.STNO', 'V', chs(1))
    call celces(modele//'.TOPOFAC.OE', 'V', chs(2))
    call celces(modele//'.TOPOFAC.AI', 'V', chs(3))
    call celces(modele//'.TOPOFAC.CF', 'V', chs(4))
!
    do 100 i = 1, 4
        call jeveuo(chs(i)//'.CESD', 'L', jcesd(i))
        call jeveuo(chs(i)//'.CESV', 'L', jcesv(i))
        call jeveuo(chs(i)//'.CESL', 'L', jcesl(i))
100 continue
!
! --- CHAMPS ELEM XFEM MULTI-HEAVISIDE
!
    lmulti = .false.
    call jeexin(modele//'.FISSNO    .CELD', ier)
    if (ier .ne. 0) then
        lmulti = .true.
        call celces(modele//'.FISSNO', 'V', chs(5))
        call celces(modele//'.HEAVNO', 'V', chs(6))
        call celces(modele//'.TOPONO.HFA', 'V', chs(7))
        do 110 i = 5, 7
            call jeveuo(chs(i)//'.CESD', 'L', jcesd(i))
            call jeveuo(chs(i)//'.CESV', 'L', jcesv(i))
            call jeveuo(chs(i)//'.CESL', 'L', jcesl(i))
110     continue
    endif
!
! --- CHAMPS ELEM XFEM TOPOLOGIE DES FONCTIONS HEAVISIDE
!
    call celces(modele//'.TOPONO.HNO', 'V', chs(8))
    call jeveuo(chs(8)//'.CESD', 'L', jcesd(8))
    call jeveuo(chs(8)//'.CESV', 'L', jcesv(8))
    call jeveuo(chs(8)//'.CESL', 'L', jcesl(8))
!
! --- LIGREL DES ELEMENTS TARDIFS DE CONTACT/FROTTEMENT
!
    ligrxf = zk24(jnosdc+3-1)(1:19)
!
! --- INITIALISATION DES CARTES POUR ELEMENTS TARDIFS
!
    carte(1) = resoco(1:14)//'.XFPO'
    carte(2) = resoco(1:14)//'.XFST'
    carte(3) = resoco(1:14)//'.XFPI'
    carte(4) = resoco(1:14)//'.XFAI'
    carte(5) = resoco(1:14)//'.XFCF'
    carte(6) = resoco(1:14)//'.XFHF'
    carte(7) = resoco(1:14)//'.XFPL'
    carte(8) = resoco(1:14)//'.XFHN'
!
    do 120 i = 1, nbch
        call detrsd('CARTE', carte(i))
        if (i .eq. 1 .or. i .eq. 3) then
            nomgd = 'N120_R'
        else if (i .eq. 2 .or. i .eq. 6 .or. i .eq. 8) then
            nomgd = 'N120_I'
        else if (i .eq. 4) then
            nomgd = 'N480_R'
        elseif (i .eq. 5) then
            nomgd = 'N120_I'
        else
            nomgd = 'NEUT_I'
        endif
        call alcart('V', carte(i), noma, nomgd)
        call jeveuo(carte(i)//'.NCMP', 'E', jncmp(i))
        call jeveuo(carte(i)//'.VALV', 'E', jvalv(i))
        do 130,k = 1,ncmp(i)
        call codent(k, 'G', ch3)
        zk8(jncmp(i)-1+k) = 'X'//ch3
130      continue
120  continue
!
! --- REMPLISSAGE DES CARTES
!
    do 200 ipc = 1, ntpc
        izone = nint(zr(jtabf+ztabf*(ipc-1)+15))
! ----- NUMEROS MAILLE ET NOMBRE DE NOEUDS ESCLAVE ET MAITRE
        nummae = nint(zr(jtabf+ztabf*(ipc-1)+1))
        nummam = nint(zr(jtabf+ztabf*(ipc-1)+2))
        nnoe = zi(jconx+nummae) - zi(jconx+nummae-1)
        nnom = zi(jconx+nummam) - zi(jconx+nummam-1)
! ----- NOMBRE DE POINTS D'INTERSECTION ET DE FACETTES ESCLAVE
        npte = nint(zr(jtabf+ztabf*(ipc-1)+24))
        ninter = nint(zr(jtabf+ztabf*(ipc-1)+14))
        nbpi=ninter
        if (nnoe .eq. nnom .and. ndim .eq. 2) then
            if (nnoe .eq. 6 .and. ninter .eq. 2) nbpi=3
            if (nnoe .eq. 8 .and. ninter .eq. 2) nbpi=3
        endif
        nface = nint(zr(jtabf+ztabf*(ipc-1)+26))
! ----- NUMERO LOCALE DE FISSURE ESCLAVE ET MAITRE
        ifise= nint(zr(jtabf+ztabf*(ipc-1)+33))
        ifism= nint(zr(jtabf+ztabf*(ipc-1)+34))
! ----- NOMBRE DE FONCTIONS HEAVISIDE
        if (lmulti) then
            nfhe = zi(jcesd(5)-1+5+4*(nummae-1)+2)
            nfhm = zi(jcesd(5)-1+5+4*(nummam-1)+2)
        else
            nfhe = 1
            nfhm = 1
        endif
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.POINT
!
        zr(jvalv(1)-1+1) = zr(jtabf+ztabf*(ipc-1)+3)
        zr(jvalv(1)-1+2) = zr(jtabf+ztabf*(ipc-1)+4)
        zr(jvalv(1)-1+3) = zr(jtabf+ztabf*(ipc-1)+5)
        zr(jvalv(1)-1+4) = zr(jtabf+ztabf*(ipc-1)+6)
        zr(jvalv(1)-1+5) = zr(jtabf+ztabf*(ipc-1)+7)
        zr(jvalv(1)-1+6) = zr(jtabf+ztabf*(ipc-1)+8)
        zr(jvalv(1)-1+7) = zr(jtabf+ztabf*(ipc-1)+9)
        zr(jvalv(1)-1+8) = zr(jtabf+ztabf*(ipc-1)+10)
        zr(jvalv(1)-1+9) = zr(jtabf+ztabf*(ipc-1)+11)
        zr(jvalv(1)-1+10) = zr(jtabf+ztabf*(ipc-1)+12)
        zr(jvalv(1)-1+11) = zr(jtabf+ztabf*(ipc-1)+13)
        zr(jvalv(1)-1+12) = npte
        zr(jvalv(1)-1+13) = mminfr(defico,'COEF_AUGM_CONT' ,izone )
        zr(jvalv(1)-1+14) = mminfr(defico,'COEF_AUGM_FROT' ,izone )
        zr(jvalv(1)-1+15) = mminfr(defico,'COEF_COULOMB' ,izone )
        zr(jvalv(1)-1+16) = mminfi(defico,'FROTTEMENT_ZONE',izone )
        zr(jvalv(1)-1+17) = zr(jtabf+ztabf*(ipc-1)+22)
        zr(jvalv(1)-1+18) = zr(jtabf+ztabf*(ipc-1)+30)
        zr(jvalv(1)-1+19) = zr(jtabf+ztabf*(ipc-1)+16)
        zr(jvalv(1)-1+20) = zr(jtabf+ztabf*(ipc-1)+29)
        zr(jvalv(1)-1+21) = zr(jtabf+ztabf*(ipc-1)+28)
        zr(jvalv(1)-1+22) = zr(jtabf+ztabf*(ipc-1)+25)
        zr(jvalv(1)-1+23) = zr(jtabf+ztabf*(ipc-1)+31)
        zr(jvalv(1)-1+24) = zr(jtabf+ztabf*(ipc-1)+17)
        zr(jvalv(1)-1+25) = zr(jtabf+ztabf*(ipc-1)+18)
        zr(jvalv(1)-1+26) = zr(jtabf+ztabf*(ipc-1)+19)
        zr(jvalv(1)-1+27) = zr(jtabf+ztabf*(ipc-1)+20)
        zr(jvalv(1)-1+28) = zr(jtabf+ztabf*(ipc-1)+21)
        zr(jvalv(1)-1+29) = zr(jtabf+ztabf*(ipc-1)+23)
        zr(jvalv(1)-1+30) = zr(jtabf+ztabf*(ipc-1)+27)
        zr(jvalv(1)-1+31) = ninter
        zr(jvalv(1)-1+33) = mminfr(defico,'COEF_PENA_CONT' ,izone )
        zr(jvalv(1)-1+34) = mminfr(defico,'COEF_PENA_FROT' ,izone )
!
        call nocart(carte(1), -3, ncmp(1), ligrel=ligrxf, nma=1,&
                    limanu=[-ipc])
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.STANO
!
        do 210 i = 1, nnoe
            do 220 j = 1, nfhe
                jfiss = 1
                if (lmulti) then
                    call cesexi('C', jcesd(5), jcesl(5), nummae, i,&
                                j, 1, iad)
                    if (iad .gt. 0) jfiss = zi(jcesv(5)-1+iad)
                endif
                call cesexi('S', jcesd(1), jcesl(1), nummae, i,&
                            jfiss, 1, iad)
                ASSERT(iad.gt.0)
                zi(jvalv(2)-1+nfhe*(i-1)+j)=zi(jcesv(1)-1+iad)
220         continue
210     continue
        do 230 i = 1, nnom
            do 240 j = 1, nfhm
                jfiss = 1
                if (lmulti) then
                    call cesexi('C', jcesd(5), jcesl(5), nummam, i,&
                                j, 1, iad)
                    if (iad .gt. 0) jfiss = zi(jcesv(5)-1+iad)
                endif
                call cesexi('S', jcesd(1), jcesl(1), nummam, i,&
                            jfiss, 1, iad)
                ASSERT(iad.gt.0)
                zi(jvalv(2)-1+nfhe*nnoe+nfhm*(i-1)+j)=zi(jcesv(1)-1+&
                iad)
240         continue
230     continue
        call nocart(carte(2), -3, ncmp(2), ligrel=ligrxf, nma=1,&
                    limanu=[-ipc])
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.PINTER
!
        do 10 i = 1, ndim
            do 20 j = 1, nbpi
                call cesexi('S', jcesd(2), jcesl(2), nummae, 1,&
                            ifise, ndim*(j-1)+i, iad)
                ASSERT(iad.gt.0)
                zr(jvalv(3)-1+ndim*(j-1)+i)=zr(jcesv(2)-1+iad)
 20         continue
 10     continue
        call nocart(carte(3), -3, ncmp(3), ligrel=ligrxf, nma=1,&
                    limanu=[-ipc])
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.AINTER
!
        do 40 i = 1, zxain
            do 50 j = 1, ninter
                call cesexi('S', jcesd(3), jcesl(3), nummae, 1,&
                            ifise, zxain*(j-1)+i, iad)
                ASSERT(iad.gt.0)
                zr(jvalv(4)-1+zxain*(j-1)+i)=zr(jcesv(3)-1+iad)
 50         continue
 40     continue
        call nocart(carte(4), -3, ncmp(4), ligrel=ligrxf, nma=1,&
                    limanu=[-ipc])
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.CCFACE
!
        do 70 i = 1, npte
            do 80 j = 1, nface
                call cesexi('S', jcesd(4), jcesl(4), nummae, 1,&
                            ifise, npte*(j-1)+i, iad)
                ASSERT(iad.gt.0)
                zi(jvalv(5)-1+npte*(j-1)+i)=zi(jcesv(4)-1+iad)
 80         continue
 70     continue
        call nocart(carte(5), -3, ncmp(5), ligrel=ligrxf, nma=1,&
                    limanu=[-ipc])
!
        if (lmulti) then
            if (nfhe .gt. 1 .or. nfhm .gt. 1) then
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.TOPONO.HFA
                ncmpe = zi(jcesd(7)-1+5+4*(nummae-1)+3)
                if (ncmpe.ge.2) then
                     call cesexi('S', jcesd(7), jcesl(7), nummae, 1,&
                                 ifise, 1, iad)
                     ASSERT(iad.gt.0)
                     zi(jvalv(6)-1+1)=zi(jcesv(7)-1+iad)
                else
                     zi(jvalv(6)-1+1)=xcalc_code(1,[-1])
                endif
                ncmpm = zi(jcesd(7)-1+5+4*(nummam-1)+3)
                if (ncmpm.ge.2) then
                     call cesexi('S', jcesd(7), jcesl(7), nummam, 1,&
                                 ifism, 2, iad)
                     ASSERT(iad.gt.0)
                     zi(jvalv(6)-1+2)=zi(jcesv(7)-1+iad)
                else
                     zi(jvalv(6)-1+2)=xcalc_code(1,[+1])
                endif
!
                call nocart(carte(6), -3, ncmp(6), ligrel=ligrxf, nma=1,&
                            limanu=[-ipc])
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.PLALA
!
                do 290 i = 1, nnoe
                    call cesexi('C', jcesd(6), jcesl(6), nummae, i,&
                                ifise, 1, iad)
                    if (iad .gt. 0) then
                        zi(jvalv(7)-1+i)=zi(jcesv(6)-1+iad)
                    else
                        zi(jvalv(7)-1+i)=1
                    endif
290             continue
                call nocart(carte(7), -3, ncmp(7), ligrel=ligrxf, nma=1,&
                            limanu=[-ipc])
            endif
        endif
!
! ----- REMPLISSAGE DE LA CARTE CARTCF.TOPONO.HNO
!
        ncmpe = zi(jcesd(8)-1+5+4*(nummae-1)+3)
        do i = 1, nnoe
            do j = 1, nfhe
                call cesexi('C', jcesd(8), jcesl(8), nummae, i,&
                                1, j, iad)
                ASSERT(iad.gt.0)
                zi(jvalv(8)-1+nfhe*(i-1)+j)=zi(jcesv(8)-1+iad)
            enddo
!   BRICOLAGE POUR REMPLIR LE IFLAG DE XCALC_HEAV QUI NE SERT QU'EN MONO HEAVISIDE
            if (nfhe.gt.0) then
            call cesexi('C', jcesd(8), jcesl(8), nummae, i,&
                                1, ncmpe, iad)
            ASSERT(iad.gt.0)
            zi(jvalv(8)-1+nfhe*nnoe+nfhm*nnom+i)=zi(jcesv(8)-1+iad)
            endif
        enddo
!
        ncmpm = zi(jcesd(8)-1+5+4*(nummam-1)+3)
        do i = 1, nnom
            do j = 1, nfhm
                call cesexi('C', jcesd(8), jcesl(8), nummam, i,&
                                1, j, iad)
                ASSERT(iad.gt.0)
                zi(jvalv(8)-1+nfhe*nnoe+nfhm*(i-1)+j)=zi(jcesv(8)-1+iad)
            enddo
!   BRICOLAGE POUR REMPLIR LE IFLAG DE XCALC_HEAV QUI NE SERT QU'EN MONO HEAVISIDE
            if (nfhm.gt.0) then
             call cesexi('C', jcesd(8), jcesl(8), nummam, i,&
                                1, ncmpm, iad)
             ASSERT(iad.gt.0)
             zi(jvalv(8)-1+(1+nfhe)*nnoe+nfhm*nnom+i)=zi(jcesv(8)-1+iad)
            endif
        enddo
!
        call nocart(carte(8), -3, ncmp(8), ligrel=ligrxf, nma=1,&
                    limanu=[-ipc])
!
        if (niv .ge. 2) then
            call xmimp3(ifm, noma, ipc, jvalv(1), jtabf)
        endif
!
200 end do
!
! --- MENAGE
!
    do 140 i = 1, nbch
        call jeexin(chs(i)//'.CESD', ier)
        if (ier .ne. 0) call detrsd('CHAM_ELEM_S', chs(i))
140 end do
!
    do 150 i = 1, nbch
        call jedetr(carte(i)//'.NCMP')
        call jedetr(carte(i)//'.VALV')
150 end do
!
    call jedema()
end subroutine
