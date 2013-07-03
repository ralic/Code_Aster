subroutine op0145()
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
    implicit none
!-----------------------------------------------------------------------
!
!     OPERATEUR "DEFI_SPEC_TURB"
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: ibid, dim, mxval
    character(len=1) :: typspe
    character(len=8) :: k8b, k8bid, intspe, caelem, modele, nomzon
    character(len=16) :: concep, cmd, nommcf, mcfac(9)
    character(len=19) :: nomu
    character(len=24) :: vain, vare, vate, nnoe, chnumi
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: iangl, ifo, ifonct, iinter, imc, imcf, imci
    integer :: inat, inatur, inoeud, iocc, ispect, jvavf
    integer :: lfon, lnat, lnnoe, lnom, long, lvain, lvare
    integer :: lvate, nbmcl, nnap
    real(kind=8) :: rbid
!-----------------------------------------------------------------------
    data mcfac /'SPEC_LONG_COR_1','SPEC_LONG_COR_2',&
     &            'SPEC_LONG_COR_3','SPEC_LONG_COR_4',&
     &            'SPEC_CORR_CONV_1','SPEC_CORR_CONV_2',&
     &            'SPEC_CORR_CONV_3',&
     &            'SPEC_FONC_FORME','SPEC_EXCI_POINT'/
! ----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
!
    do 10 imcf = 1, 9
        call getfac(mcfac(imcf), iocc)
        if (iocc .eq. 1) goto 11
10  end do
11  continue
!     NBMCL EST AFFECTE A 12 , IL DOIT ETRE SUPERIEUR AU MAX DES NOMBRE
!     DE MC SIMPLES DES 9 MC FACTEURS
    nbmcl = 12
!
    nommcf=mcfac(imcf)
!
    read(nommcf(6:6),'(A1)') typspe
    if (typspe .eq. 'L') then
        read(nommcf(15:15),'(I1)') ispect
    else if (typspe.eq.'F') then
        ispect = 11
    else if (typspe.eq.'C') then
        read(nommcf(16:16),'(I1)') ispect
    else
        ispect = 21
    endif
!
! ----VERIFICATIONS AVANT EXECUTION----
!     =============================
!
    if (ispect .eq. 21) then
        call getvid(nommcf, 'INTE_SPEC', 1, iarg, 0,&
                    k8bid, iinter)
        if (iinter .ne. 0) then
            call getvtx(nommcf, 'NATURE', 1, iarg, 0,&
                        k8bid, inatur)
            call getvr8(nommcf, 'ANGLE', 1, iarg, 0,&
                        rbid, iangl)
            call getvtx(nommcf, 'NOEUD', 1, iarg, 0,&
                        k8bid, inoeud)
            if (inatur .ne. iangl .or. inatur .ne. inoeud .or. inoeud .ne. iangl) then
                call u2mess('F', 'MODELISA5_66')
            endif
        else
            call getvtx(nommcf, 'NOEUD', 1, iarg, 0,&
                        k8bid, inoeud)
            if (abs(inoeud) .ne. 1) then
                call u2mess('F', 'MODELISA5_67')
            endif
        endif
    endif
!
! ----FIN DES VERIFICATIONS AVANT EXECUTION----
!     =====================================
!
! ----VERIFICATIONS A L'EXECUTION----
!     ===========================
!
    if (ispect .eq. 11 .or. ispect .eq. 21) then
        call getvid(nommcf, 'INTE_SPEC', 1, iarg, 0,&
                    k8bid, iinter)
        if (iinter .ne. 0) then
            call getvid(nommcf, 'INTE_SPEC', 1, iarg, 1,&
                        intspe, ibid)
            chnumi = intspe//'.NUMI'
            call jelira(chnumi, 'LONMAX', mxval, k8b)
            if (ispect .eq. 11) then
                call getvid(nommcf, 'FONCTION', 1, iarg, 0,&
                            k8bid, ifonct)
                dim = abs(ifonct)
                dim = dim*(dim+1)/2
                if (dim .ne. mxval) then
                    call u2mess('F', 'MODELISA5_68')
                endif
            else
                call getvtx(nommcf, 'NOEUD', 1, iarg, 0,&
                            k8bid, inoeud)
                dim = abs(inoeud)
                dim = dim*(dim+1)/2
                if (dim .ne. mxval) then
                    call u2mess('F', 'MODELISA5_69')
                endif
            endif
        endif
    endif
!
! ----FIN DES VERIFICATIONS A L'EXECUTION----
!     ===================================
!
! ----CREATION DES OBJETS ET REMPLISSAGE EN FONCTION DES----
! ----          DIFFERENTS TYPES DE SPECTRE             ----
!     ==================================================
!
! ----0.DENOMINATIONS DES OBJETS A CREER EN GLOBALE
!       -------------------------------------------
!
    vain = nomu//'.VAIN'
    vare = nomu//'.VARE'
    vate = nomu//'.VATE'
    nnoe = nomu//'.NNOE'
!
!
! ----1.MODELES "LONGUEUR DE CORRELATION"
!       ---------------------------------
!
    if (ispect .lt. 10 .or. nommcf(1:14) .eq. 'SPEC_CORR_CONV') then
!
! ------1.1.CREATION DES OBJETS SUR LA BASE GLOBALE
!
        call wkvect(vain, 'G V I', 1, lvain)
        call wkvect(vare, 'G V R', nbmcl, lvare)
        long = nbmcl + 1
        call wkvect(vate, 'G V K16', long, lvate)
        call wkvect(nomu//'.VAVF', 'G V K8', 1, jvavf)
!
! ------1.2.CREATION D'OBJETS SUR LA BASE VOLATILE
!
        call wkvect('OP0145.TEMP.NOM', 'V V K16', nbmcl, lnom)
!
! ------1.3.REMPLISSAGE DES OBJETS
!
        if (nommcf .eq. 'SPEC_LONG_COR_1') then
            zk16(lnom)      ='LONG_COR        '
            zk16(lnom+1)    ='PROF_VITE_FLUI  '
            zk16(lnom+2)    ='VISC_CINE       '
        else if (nommcf.eq.'SPEC_LONG_COR_2') then
            zk16(lnom)      ='LONG_COR        '
            zk16(lnom+1)    ='PROF_VITE_FLUI  '
            zk16(lnom+2)    ='FREQ_COUP       '
            zk16(lnom+3)    ='PHI0            '
            zk16(lnom+4)    ='BETA            '
        else if (nommcf.eq.'SPEC_LONG_COR_3') then
            zk16(lnom)      ='LONG_COR        '
            zk16(lnom+1)    ='PROF_VITE_FLUI  '
            zk16(lnom+2)    ='FREQ_COUP       '
            zk16(lnom+3)    ='PHI0_1          '
            zk16(lnom+4)    ='BETA_1          '
            zk16(lnom+5)    ='PHI0_2          '
            zk16(lnom+6)    ='BETA_2          '
        else if (nommcf.eq.'SPEC_LONG_COR_4') then
            zk16(lnom)      ='LONG_COR        '
            zk16(lnom+1)    ='PROF_VITE_FLUI  '
            zk16(lnom+2)    ='TAUX_VIDE       '
            zk16(lnom+3)    ='BETA            '
            zk16(lnom+4)    ='GAMMA           '
        else if (nommcf.eq.'SPEC_CORR_CONV_1') then
            zk16(lnom)      ='LONG_COR_1      '
            zk16(lnom+1)    ='LONG_COR_2      '
            zk16(lnom+2)    ='VITE_FLUI       '
            zk16(lnom+3)    ='RHO_FLUI        '
            zk16(lnom+4)    ='FREQ_COUP       '
            zk16(lnom+5)    ='K               '
            zk16(lnom+6)    ='D_FLUI          '
            zk16(lnom+7) ='COEF_VITE_FLUI_A'
            zk16(lnom+8) ='COEF_VITE_FLUI_O'
            zk16(lnom+9)    ='METHODE         '
        else if (nommcf.eq.'SPEC_CORR_CONV_2') then
            zk16(lnom)      ='FONCTION        '
            zk16(lnom+1)    ='VITE_FLUI       '
            zk16(lnom+2)    ='FREQ_COUP       '
            zk16(lnom+3)    ='METHODE         '
            zk16(lnom+4) ='COEF_VITE_FLUI_A'
            zk16(lnom+5) ='COEF_VITE_FLUI_O'
        else if (nommcf.eq.'SPEC_CORR_CONV_3') then
            zk16(lnom)      ='TABLE_FONCTION  '
        else if (nommcf.eq.'SPEC_FONC_FORME') then
            zk16(lnom)      ='INTE_SPEC       '
            zk16(lnom+1)    ='FONCTION        '
            zk16(lnom+2)    ='GRAPPE_1        '
            zk16(lnom+3)    ='NOEUD           '
            zk16(lnom+4)    ='CARA_ELEM       '
            zk16(lnom+5)    ='MODELE          '
        else if (nommcf.eq.'SPEC_EXCI_POINT') then
            zk16(lnom)      ='INTE_SPEC      '
            zk16(lnom+1)    ='NATURE         '
            zk16(lnom+2)    ='ANGL           '
            zk16(lnom+3)    ='GRAPPE_2       '
            zk16(lnom+4)    ='RHO_FLUI       '
            zk16(lnom+5)    ='NOEUD          '
            zk16(lnom+6)    ='CARA_ELEM      '
            zk16(lnom+7)    ='MODELE         '
        endif
        zk16(lvate) = nommcf
        imci = 0
        do 20 imc = 1, nbmcl
            if (zk16(lnom+imc-1) .eq. 'PROF_VITE_FLUI  ') then
                call getvid(nommcf, 'PROF_VITE_FLUI', 1, iarg, 1,&
                            nomzon, ibid)
                zk16(lvate+imc) = nomzon
                zk8(jvavf)=nomzon
            else if (zk16(lnom+imc-1).eq.'METHODE         ') then
                call getvtx(nommcf, 'METHODE', 1, iarg, 1,&
                            nomzon, ibid)
                zk16(lvate+imc) = nomzon
            else if (zk16(lnom+imc-1).eq.'FONCTION        ') then
                call getvid(nommcf, 'FONCTION', 1, iarg, 1,&
                            nomzon, ibid)
                zk16(lvate+imc) = nomzon
            else if (zk16(lnom+imc-1).eq.'TABLE_FONCTION  ') then
                call getvid(nommcf, 'TABLE_FONCTION', 1, iarg, 1,&
                            nomzon, ibid)
                zk16(lvate+imc) = nomzon
            else if (zk16(lnom+imc-1).ne.'                ') then
                imci = imci + 1
                zk16(lvate+imc) = zk16(lnom+imc-1)
                call getvr8(nommcf, zk16(lnom+imc-1), 1, iarg, 1,&
                            zr( lvare+imci-1), ibid)
            endif
20      continue
        zi(lvain) = ispect
!
!
! ----2.MODELES "FONCTIONS DE FORME" ET "EXCITATIONS PONCTUELLES"
!       ---------------------------------------------------------
!
    else
!
! ------2.1.CREATION ET REMPLISSAGE D'OBJETS COMMUNS
!
! ------2.1.1.OBJET .VAIN
!
        call wkvect(vain, 'G V I', 3, lvain)
        call getvtx(nommcf, 'NOEUD', 1, iarg, 0,&
                    k8bid, nnap)
        nnap = abs(nnap)
        zi(lvain) = ispect
        if (iinter .eq. 0) then
            zi(lvain+1) = 1
        else
            zi(lvain+1) = 0
        endif
        zi(lvain+2) = nnap
!
! ------2.1.2.OBJET .NNOE
!
        call wkvect(nnoe, 'G V K8', nnap, lnnoe)
        call getvtx(nommcf, 'NOEUD', 1, iarg, nnap,&
                    zk8(lnnoe), ibid)
!
! ------2.2.OBJETS .VATE ET .VARE
!
        long = 5
        if (iinter .ne. 0) then
            if (ispect .eq. 11) then
                ifonct = abs(ifonct)
                long = 4 + ifonct
            else
                inoeud = abs(inoeud)
                long = 4 + inoeud
            endif
        endif
!
        call wkvect(vate, 'G V K16', long, lvate)
        call getvid(nommcf, 'CARA_ELEM', 1, iarg, 1,&
                    caelem, ibid)
        call getvid(nommcf, 'MODELE', 1, iarg, 1,&
                    modele, ibid)
        zk16(lvate) = nommcf
        zk16(lvate+1) = caelem
        zk16(lvate+2) = modele
!
! ------2.2.1.MODELE "FONCTIONS DE FORME"
!
        if (ispect .eq. 11) then
            if (iinter .eq. 0) then
                zk16(lvate+3) = 'GRAPPE_1'
                call getvtx(nommcf, 'GRAPPE_1', 1, iarg, 1,&
                            zk16(lvate+4), ibid)
            else
                call wkvect('OP0145.TEMP.FON', 'V V K8', ifonct, lfon)
                call getvid(nommcf, 'FONCTION', 1, iarg, ifonct,&
                            zk8(lfon), ibid)
                zk16(lvate+3) = intspe
                do 30 ifo = 1, ifonct
                    zk16(lvate+3+ifo) = zk8(lfon+ifo-1)
30              continue
            endif
!
! ------2.2.2.MODELE "EXCITATIONS PONCTUELLES"
!
        else
            if (iinter .eq. 0) then
                zk16(lvate+3) = 'GRAPPE_2'
                call getvtx(nommcf, 'GRAPPE_2', 1, iarg, 1,&
                            zk16(lvate+4), ibid)
!
                call wkvect(vare, 'G V R', 1, lvare)
                call getvr8(nommcf, 'RHO_FLUI', 1, iarg, 1,&
                            zr(lvare), ibid)
!
            else
                call wkvect('OP0145.TEMP.NAT', 'V V K8', inoeud, lnat)
                call getvtx(nommcf, 'NATURE', 1, iarg, inoeud,&
                            zk8(lnat), ibid)
                zk16(lvate+3) = intspe
                do 40 inat = 1, inoeud
                    zk16(lvate+3+inat) = zk8(lnat+inat-1)
40              continue
!
                call wkvect(vare, 'G V R', inoeud, lvare)
                call getvr8(nommcf, 'ANGLE', 1, iarg, inoeud,&
                            zr(lvare), ibid)
!
            endif
!
        endif
!
    endif
!
    call titre()
!
    call jedema()
end subroutine
