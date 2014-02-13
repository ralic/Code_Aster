subroutine specff(casint, nomu, spectr, base, nuor,&
                  imodi, imodf, nbm, nbpf)
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
!     PROJECTION D'UN SPECTRE D'EXCITATION TURBULENTE REPARTIE (AVEC
!     FONCTIONS DE FORME) SUR UNE BASE MODALE PERTURBEE PAR PRISE EN
!     COMPTE DU COUPLAGE FLUIDE STRUCTURE
!     APPELANT : OP0146 , OPERATEUR PROJ_SPEC_BASE
!-----------------------------------------------------------------------
! IN  : CASINT  : BOOLEEN, DONNE L'OPTION DE CALCUL
!       CASINT  = .TRUE.  => CALCUL DE TOUS LES INTERSPECTRES
!       CASINT  = .FALSE. => CALCUL DES AUTOSPECTRES UNIQUEMENT
! IN  : NOMU    : NOM UTILISATEUR
! IN  : SPECTR  : NOM DU CONCEPT SPECTRE
! IN  : BASE    : NOM DU CONCEPT MELASFLU
! IN  : NUOR    : NUMEROS D'ORDRE DES MODES DU CONCEPT MELASFLU
! IN  : IMODI   : INDICE DU PREMIER MODE PRIS EN COMPTE
! IN  : IMODF   : INDICE DU DERNIER MODE PRIS EN COMPTE
! IN  : NBM     : NOMBRE DE MODES DU CONCEPT MELASFLU
! IN  : NBPF    : NOMBRE DE POINTS DE LA DISCRETISATION FREQUENTIELLE
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/axdipo.h"
#include "asterfort/deffen.h"
#include "asterfort/discax.h"
#include "asterfort/discff.h"
#include "asterfort/fenexc.h"
#include "asterfort/ffgra1.h"
#include "asterfort/fointr.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgauss.h"
#include "asterfort/pha180.h"
#include "asterfort/pha300.h"
#include "asterfort/scaldf.h"
#include "asterfort/scalff.h"
#include "asterfort/utmess.h"
#include "asterfort/veriff.h"
#include "asterfort/wkvect.h"
!
    logical :: casint, ltable, exiind
    integer :: imodi, imodf, nbm, nuor(nbm), nbpf, ival(2)
    character(len=8) :: nomu, caelem, modele, table, nomnoa, noma
    character(len=19) :: spectr, base, typflu
!
    integer :: ibid, dim, dimint, mxval, itab, nbfreq, nbval, ifreq
    integer :: ire, iim, isre, isim, i1, ind, lnumi, lnumj, ier2, ij, iprol
    integer :: iaxe, ichref, ideb, idebit, idec, idefm, idiax
    integer :: idife, idiff, ifo, ifo1, ifo2, ifoi, ifsic, iinte, il, im1
    integer :: imata, imatb, inofe, inomf, inuno, iremf, iret, im1b, im2, im2b
    integer :: ispin, ispno, ispte, itypfl, iv, ivaff, ivale
    integer :: lwr, nbfonc, nbmr, nbn, nbnfen, nbp, nbp1, nbp2
    real(kind=8) :: r8b, long, module, pla180(21), pla300(21), det
    real(kind=8) :: beta11, beta12, beta21, beta22, phase, ptf, s0
    character(len=24) :: spvain, spvate, spnnoe
    character(len=24) :: remf, fsic, chrefe, chvale, chnumi, chnumj, chtab
!
    data pla180 / 8.d-1  , 1.d0   , 1.3d0  , 1.5d-1 , 2.d-1  ,&
     &              4.d-2  , 2.4d-1 , 3.4d-1 , 6.d-2  , 2.8d-1 ,&
     &              1.d-1  , 2.d-2  , 3.d-2  , 1.3d-1 , 4.2d-1 ,&
     &              1.d0   , 1.3d0  , 1.8d-1 , 6.1d-1 , 2.5d-1 ,&
     &              2.6d0  /
!
    data pla300 / 1.1d0  , 1.6d0  , 2.6d0  , 1.8d-1 , 3.2d-1 ,&
     &              4.d-2  , 1.6d-1 , 2.7d-1 , 4.d-2  , 1.d-1  ,&
     &              4.9d-1 , 8.3d-1 , 3.d-2  , 7.d-2  , 8.8d-1 ,&
     &              1.7d0  , 3.d0   , 3.d-1  , 3.2d-1 , 1.d0   ,&
     &              4.5d0  /
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
! --- 1.TEST DE COMPATIBILITE TYPE DE SPECTRE/CONFIGURATION ETUDIEE ---
!
    remf = base//'.REMF'
    call jeveuo(remf, 'L', iremf)
    typflu = zk8(iremf)
    fsic = typflu//'.FSIC'
    call jeveuo(fsic, 'L', ifsic)
    itypfl = zi(ifsic)
    if (itypfl .ne. 2) then
        call utmess('F', 'MODELISA7_4')
    endif
!
! --- 2.RECUPERATION DES INFORMATIONS CARACTERISTIQUES DU SPECTRE ---
!
    spvain = spectr//'.VAIN'
    spvate = spectr//'.VATE'
    spnnoe = spectr//'.NNOE'
!
    call jeveuo(spvain, 'L', ispin)
    ltable = .false.
    if (zi(ispin+1) .eq. 0) ltable = .true.
!
    call jeveuo(spvate, 'L', ispte)
    caelem = zk16(ispte+1)(1:8)
    modele = zk16(ispte+2)(1:8)
    if (ltable) then
        table = zk16(ispte+3)(1:8)
        chnumi = table//'.NUMI'
        chnumj = table//'.NUMJ'
        call jeveuo(chnumi, 'L', lnumi)
        call jeveuo(chnumj, 'L', lnumj)
        call jelira(chnumi, 'LONMAX', mxval)
        r8b = (-1.d0+sqrt(1.d0+8.d0*mxval))/2.d0
        nbfonc = int(r8b)
        call wkvect('&&SPECFF.TEMP.NOMF', 'V V K8', nbfonc, inomf)
        do 10 ifo = 1, nbfonc
            zk8(inomf+ifo-1) = zk16(ispte+3+ifo)(1:8)
10      continue
    else
        nbfonc = 12
        dimint = 6
        read(zk16(ispte+4)(7:9),'(I3)') idebit
    endif
!
    call jeveuo(spnnoe, 'L', ispno)
    nomnoa = zk8(ispno)
!
! --- 3.RECUPERATION DU NOM DU CONCEPT MAILLAGE ---
!
    iv = 1
    write(chrefe,'(A8,A5,2I3.3,A5)') base(1:8),'.C01.',nuor(1),iv,&
     &                                 '.REFE'
    call jeveuo(chrefe, 'L', ichref)
    noma = zk24(ichref)(1:8)
!
! --- 4.DISCRETISATION DES FONCTIONS DE FORME ---
!
    if (ltable) then
        call veriff(nbfonc, zk8(inomf), nbp1, nbp2, long)
        nbp = nbp1 + nbp2
        call wkvect('&&SPECFF.TEMP.DIFF', 'V V R', nbp, idiff)
        call wkvect('&&SPECFF.TEMP.VAFF', 'V V R', nbp*nbfonc, ivaff)
        call discff(nbfonc, zk8(inomf), nbp1, nbp2, zr(idiff),&
                    zr(ivaff))
    else
        nbp1 = 101
        nbp2 = nbp1
        nbp = nbp1 + nbp2
        long = 0.971d0
        call wkvect('&&SPECFF.TEMP.DIFF', 'V V R', nbp, idiff)
        call wkvect('&&SPECFF.TEMP.VAFF', 'V V R', nbp*nbfonc, ivaff)
        call ffgra1(nbfonc, idebit, nbp1, nbp2, long,&
                    zr(idiff), zr(ivaff))
    endif
!
! --- 5.CARACTERISATION DU MAILLAGE DE LA POUTRE ---
!
! --- 5.1.DETERMINATION DE L'AXE DIRECTEUR DE LA POUTRE
!
    call axdipo(noma, caelem, modele, iaxe)
!
! --- 5.2.CREATION D'UNE LISTE ORDONNEE DE NOEUDS : ORDRE CROISSANT
! ---     DU PARAMETRE LE LONG DE L'AXE DIRECTEUR DE LA POUTRE
!
    call jelira(noma//'.NOMNOE', 'NOMUTI', nbn)
    if (nbn .lt. 3) then
        call utmess('F', 'MODELISA7_6')
    endif
    call wkvect('&&SPECFF.TEMP.NUNO', 'V V I', nbn, inuno)
    call wkvect('&&SPECFF.TEMP.DIAX', 'V V R', nbn, idiax)
    call discax(noma, nbn, iaxe, zi(inuno), zr(idiax))
!
! --- 6.EXTRACTION DE LA FENETRE EXCITEE SUR LE MAILLAGE : LISTE DES ---
! ---   NOEUDS ET DISCRETISATION SPATIALE CORRESPONDANTE, TRANSLATEE ---
! ---   POUR ETRE SUPERPOSEE A CELLE DES FONCTIONS DE FORME          ---
!
    call wkvect('&&SPECFF.TEMP.NOFE', 'V V I', nbn, inofe)
    call wkvect('&&SPECFF.TEMP.DIFE', 'V V R', nbn, idife)
    call fenexc(noma, nomnoa, long, nbn, zi(inuno),&
                zr(idiax), nbnfen, zi(inofe), zr(idife))
!
! --- 7.EXTRACTION DES COMPOSANTES DES DEFORMEES MODALES SUR LA ---
! ---   FENETRE EXCITEE, DANS LES DEUX DIRECTIONS ORTHOGONALES  ---
! ---   A LA POUTRE                                             ---
!
    nbmr = imodf - imodi + 1
    call wkvect('&&SPECFF.TEMP.DEFM', 'V V R', nbp*nbmr, idefm)
    call deffen(base, nuor, imodi, nbmr, nbm,&
                iaxe, long, nbnfen, zi(inofe), zr(idife),&
                nbp1, nbp2, zr(idiff), zr(idefm))
!
! --- 8.CALCUL DE LA DECOMPOSITION DES DEFORMEES MODALES SUR LA ---
! ---   FAMILLE DES FONCTIONS DE FORME ASSOCIEES A L'EXCITATION ---
!
    call wkvect('&&SPECFF.TEMP.MATA', 'V V R', nbfonc*nbfonc, imata)
    call wkvect('&&SPECFF.TEMP.MATB', 'V V R', nbfonc*nbmr, imatb)
    call scalff(nbfonc, nbp, zr(idiff), zr(ivaff), zr(imata))
    call scaldf(nbfonc, nbp, nbmr, zr(idiff), zr(ivaff),&
                zr(idefm), zr(imatb))
    call mgauss('NFVP', zr(imata), zr(imatb), nbfonc, nbfonc,&
                nbmr, det, iret)
!
! --- 9.CALCUL DES INTERSPECTRES D'EXCITATIONS MODALES ---
! ---   BOUCLE SUR LE NOMBRE DE VITESSES               ---
!
    call wkvect('&&SPECFF.TEMP.SWR ', 'V V R', nbpf, lwr)
    if (ltable) then
        dim = nbfonc*(nbfonc+1)/2
    else
        dim = dimint*(dimint+1)/2
    endif
    dim = 2*nbpf*dim
    call wkvect('&&SPECFF.TEMP.INTE', 'V V R', dim, iinte)
!
! --- 9.1.RECUPERATION DE LA DISCRETISATION FREQUENTIELLE
    call jeveuo(nomu//'.ABS', 'L', lwr)
!
! --- 9.2.INTERPOLATION DES INTERSPECTRES A PROJETER
!
    if (ltable) then
        call wkvect('&&SPECFF.PROL', 'V V K24', 5, iprol)
        zk24(iprol) = 'FONCTION'
        zk24(iprol+1) = 'LIN LIN '
        zk24(iprol+2) = ' '
        zk24(iprol+3) = 'TOUTRESU'
        zk24(iprol+4) = 'CC      '
        call wkvect('&&SPECFF.IRE', 'V V R', nbpf, ire)
        call wkvect('&&SPECFF.IIM', 'V V R', nbpf, iim)
        chtab=table//'.VALE'
        call jeveuo(table//'.ABS', 'L', ifreq)
        do 60 ifo2 = 1, nbfonc
            ival(2) = ifo2
            do 61 ifo1 = 1, ifo2
                ifo = ifo2*(ifo2-1)/2 + ifo1
                ival(1) = ifo1
                exiind = .false.
                do 320 i1 = 1, mxval
                    if ((zi(lnumi-1+i1) .eq. ival(1)) .and. (zi(lnumj- 1+i1) .eq. ival(2))) then
                        exiind = .true.
                        ind = i1
                    endif
320              continue
                if (.not. exiind) then
                    call utmess('F', 'MODELISA2_89')
                endif
                call jeveuo(jexnum(chtab, ind), 'L', itab)
                call jelira(jexnum(chtab, ind), 'LONMAX', nbval)
                if (ifo2 .eq. ifo1) then
                    nbfreq = nbval
                    call jeexin('&&SPECFF.SRE', ibid)
                    if (ibid .eq. 0) then
                        call wkvect('&&SPECFF.SRE', 'V V R', nbfreq, isre)
                        call wkvect('&&SPECFF.SIM', 'V V R', nbfreq, isim)
                    endif
                    call fointr(' ', zk24(iprol), nbfreq, zr(ifreq), zr( itab),&
                                nbpf, zr(lwr), zr(isre), ier2)
                    do 210 i1 = 1, nbfreq
                        zr(isim-1+i1) = 0.d0
210                  continue
                else
                    nbfreq = nbval/2
                    call jeexin('&&SPECFF.SRE', ibid)
                    if (ibid .eq. 0) then
                        call wkvect('&&SPECFF.SRE', 'V V R', nbfreq, isre)
                        call wkvect('&&SPECFF.SIM', 'V V R', nbfreq, isim)
                    endif
                    do 310 i1 = 1, nbfreq
                        zr(ire-1+i1) = zr(itab+2*(i1-1))
                        zr(iim-1+i1) = zr(itab+2*(i1-1)+1)
310                  continue
                    call fointr(' ', zk24(iprol), nbfreq, zr(ifreq), zr( ire),&
                                nbpf, zr(lwr), zr(isre), ier2)
                    call fointr(' ', zk24(iprol), nbfreq, zr(ifreq), zr( iim),&
                                nbpf, zr(lwr), zr(isim), ier2)
                endif
                do 62 il = 1, nbpf
                    idec = 2*nbpf*(ifo-1)+2*(il-1)
                    zr(iinte+idec ) = zr(isre-1+il)
                    zr(iinte+idec+1) = zr(isim-1+il)
62              continue
61          continue
60      continue
!
    else if (idebit.eq.180) then
!
        do 70 ifo2 = 1, dimint
            ifo = ifo2*(ifo2+1)/2
            s0 = pla180(ifo)
            do 71 il = 1, nbpf
                idec = 2*nbpf*(ifo-1)+2*(il-1)
                ptf = zr(lwr+il-1)
                module = 1.d0 + (ptf/15.d0)**(4.6d0)
                module = s0/module
                zr(iinte+idec) = module
71          continue
70      continue
        do 72 ifo2 = 2, dimint
            do 73 ifo1 = 1, ifo2-1
                ifo = ifo2*(ifo2-1)/2 + ifo1
                s0 = pla180(ifo)
                ifoi = (ifo2-1)*(ifo2-2)/2 + ifo1
                do 74 il = 1, nbpf
                    idec = 2*nbpf*(ifo-1)+2*(il-1)
                    ptf = zr(lwr+il-1)
                    call pha180(ifoi, ptf, phase)
                    module = 1.d0 + (ptf/15.d0)**(4.6d0)
                    module = s0/module
                    zr(iinte+idec) = module * dble(cos(phase))
                    zr(iinte+idec+1) = module * dble(sin(phase))
74              continue
73          continue
72      continue
!
    else if (idebit.eq.300) then
!
        do 80 ifo2 = 1, dimint
            ifo = ifo2*(ifo2+1)/2
            s0 = pla300(ifo)
            do 81 il = 1, nbpf
                idec = 2*nbpf*(ifo-1)+2*(il-1)
                ptf = zr(lwr+il-1)
                module = 1.d0 + (ptf/15.d0)**(4.6d0)
                module = s0/module
                zr(iinte+idec) = module
81          continue
80      continue
        do 82 ifo2 = 2, dimint
            do 83 ifo1 = 1, ifo2-1
                ifo = ifo2*(ifo2-1)/2 + ifo1
                s0 = pla300(ifo)
                ifoi = (ifo2-1)*(ifo2-2)/2 + ifo1
                do 84 il = 1, nbpf
                    idec = 2*nbpf*(ifo-1)+2*(il-1)
                    ptf = zr(lwr+il-1)
                    call pha300(ifoi, ptf, phase)
                    module = 1.d0 + (ptf/15.d0)**(4.6d0)
                    module = s0/module
                    zr(iinte+idec) = module * dble(cos(phase))
                    zr(iinte+idec+1) = module * dble(sin(phase))
84              continue
83          continue
82      continue
!
    endif
!
! --- 9.3.PROJECTION DES INTERSPECTRES
!
    ij = 0
    chvale = nomu//'.VALE'
    do 90 im2 = imodi, imodf
        ideb = im2
        if (casint) ideb = imodi
        do 91 im1 = ideb, im2
            ij = ij + 1
            call jeveuo(jexnum(chvale, ij), 'E', ivale)
            call jelira(jexnum(chvale, ij), 'LONMAX', nbval)
            im2b = im2 - imodi + 1
            im1b = im1 - imodi + 1
!
            if (ltable) then
                do 100 il = 1, nbpf
                    do 101 ifo2 = 1, nbfonc
                        beta12 = zr(imatb+nbfonc*(im1b-1)+ifo2-1)
                        beta22 = zr(imatb+nbfonc*(im2b-1)+ifo2-1)
                        ifo = ifo2*(ifo2+1)/2
                        idec = 2*nbpf*(ifo-1)+2*(il-1)
                        if (nbval .eq. nbpf) then
                            zr(ivale+il-1) = zr(ivale+il-1) + beta12* beta22 * zr(iinte+idec)
                        else
                            zr(ivale+2*(il-1)) = zr(&
                                                 ivale+2*(il-1)) + beta12*beta22 * zr(iinte+idec)
                        endif
101                  continue
                    if (nbfonc .gt. 1) then
                        do 102 ifo2 = 2, nbfonc
                            beta12 = zr(imatb+nbfonc*(im1b-1)+ifo2-1)
                            beta22 = zr(imatb+nbfonc*(im2b-1)+ifo2-1)
                            do 103 ifo1 = 1, ifo2-1
                                beta11 = zr(imatb+nbfonc*(im1b-1)+ ifo1-1)
                                beta21 = zr(imatb+nbfonc*(im2b-1)+ ifo1-1)
                                ifo = ifo2*(ifo2-1)/2 + ifo1
                                idec = 2*nbpf*(ifo-1)+2*(il-1)
                                if (nbval .eq. nbpf) then
                                    zr(ivale+il-1) = zr(ivale+il-1)+( beta11*beta22+beta12*beta21&
                                                     &)*zr( iinte+idec)
                                else
                                    zr(ivale+2*(il-1)) = zr(&
                                                         ivale+2*( il-1))+(beta11*beta22+beta12* &
                                                         &beta21)*zr(iinte+idec&
                                                         )
                                    zr(ivale+2*(il-1)+1) = zr(&
                                                           ivale+2* (il-1)+1) + (beta11*beta22-be&
                                                           &ta12* beta21)*zr(iinte+idec+1&
                                                           )
                                endif
103                          continue
102                      continue
                    endif
100              continue
!
            else
!
                do 110 il = 1, nbpf
                    do 111 ifo2 = 1, dimint
                        beta12 = zr(imatb+nbfonc*(im1b-1)+ifo2-1)
                        beta22 = zr(imatb+nbfonc*(im2b-1)+ifo2-1)
                        ifo = ifo2*(ifo2+1)/2
                        idec = 2*nbpf*(ifo-1)+2*(il-1)
                        if (nbval .eq. nbpf) then
                            zr(ivale+il-1) = zr(ivale+il-1) + beta12* beta22 * zr(iinte+idec)
                        else
                            zr(ivale+2*(il-1)) = zr(&
                                                 ivale+2*(il-1)) + beta12*beta22 * zr(iinte+idec)
                        endif
111                  continue
                    do 112 ifo2 = dimint+1, nbfonc
                        beta12 = zr(imatb+nbfonc*(im1b-1)+ifo2-1)
                        beta22 = zr(imatb+nbfonc*(im2b-1)+ifo2-1)
                        ifo = (ifo2-dimint)*(ifo2-dimint+1)/2
                        idec = 2*nbpf*(ifo-1)+2*(il-1)
                        if (nbval .eq. nbpf) then
                            zr(ivale+il-1) = zr(ivale+il-1) + beta12* beta22 * zr(iinte+idec)
                        else
                            zr(ivale+2*(il-1)) = zr(&
                                                 ivale+2*(il-1)) + beta12*beta22 * zr(iinte+idec)
                        endif
112                  continue
                    do 113 ifo2 = 2, dimint
                        beta12 = zr(imatb+nbfonc*(im1b-1)+ifo2-1)
                        beta22 = zr(imatb+nbfonc*(im2b-1)+ifo2-1)
                        do 114 ifo1 = 1, ifo2-1
                            beta11 = zr(imatb+nbfonc*(im1b-1)+ifo1-1)
                            beta21 = zr(imatb+nbfonc*(im2b-1)+ifo1-1)
                            ifo = ifo2*(ifo2-1)/2 + ifo1
                            idec = 2*nbpf*(ifo-1)+2*(il-1)
                            if (nbval .eq. nbpf) then
                                zr(ivale+il-1) = zr(ivale+il-1)+( beta11*beta22+beta12*beta21)*zr&
                                                 &(iinte+ idec)
                            else
                                zr(ivale+2*(il-1)) = zr(&
                                                     ivale+2*(il-1) )+(beta11*beta22+beta12*beta2&
                                                     &1)*zr( iinte+idec&
                                                     )
                                zr(ivale+2*(il-1)+1) = zr(&
                                                       ivale+2*(il- 1)+1) + (beta11*beta22-beta12&
                                                       &*beta21)* zr(iinte+idec+1&
                                                       )
                            endif
114                      continue
113                  continue
                    do 115 ifo2 = dimint+2, nbfonc
                        beta12 = zr(imatb+nbfonc*(im1b-1)+ifo2-1)
                        beta22 = zr(imatb+nbfonc*(im2b-1)+ifo2-1)
                        do 116 ifo1 = dimint+1, ifo2-1
                            beta11 = zr(imatb+nbfonc*(im1b-1)+ifo1-1)
                            beta21 = zr(imatb+nbfonc*(im2b-1)+ifo1-1)
                            ifo = (ifo2-dimint)*(ifo2-dimint-1)/2 + ifo1-dimint
                            idec = 2*nbpf*(ifo-1)+2*(il-1)
                            if (nbval .eq. nbpf) then
                                zr(ivale+il-1) = zr(ivale+il-1)+( beta11*beta22+beta12*beta21)*zr&
                                                 &(iinte+ idec)
                            else
                                zr(ivale+2*(il-1)) = zr(&
                                                     ivale+2*(il-1) )+(beta11*beta22+beta12*beta2&
                                                     &1)*zr( iinte+idec&
                                                     )
                                zr(ivale+2*(il-1)+1) = zr(&
                                                       ivale+2*(il- 1)+1) + (beta11*beta22-beta12&
                                                       &*beta21)* zr(iinte+idec+1&
                                                       )
                            endif
116                      continue
115                  continue
110              continue
!
            endif
!
91      continue
90  continue
!
    call jedetr('&&SPECFF.TEMP.NOMF')
    call jedetr('&&SPECFF.TEMP.DIFF')
    call jedetr('&&SPECFF.TEMP.VAFF')
    call jedetr('&&SPECFF.TEMP.NUNO')
    call jedetr('&&SPECFF.TEMP.DIAX')
    call jedetr('&&SPECFF.TEMP.NOFE')
    call jedetr('&&SPECFF.TEMP.DIFE')
    call jedetr('&&SPECFF.TEMP.DEFM')
    call jedetr('&&SPECFF.TEMP.MATA')
    call jedetr('&&SPECFF.TEMP.MATB')
    call jedetr('&&SPECFF.TEMP.SWR ')
    call jedetr('&&SPECFF.TEMP.INTE')
!
    call jedema()
end subroutine
