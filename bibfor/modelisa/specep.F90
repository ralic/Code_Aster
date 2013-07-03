subroutine specep(casint, nomu, spectr, base, vite,&
                  nuor, imodi, imodf, nbm, nbpf)
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
!     PROJECTION D'UN SPECTRE D'EXCITATION TURBULENTE LOCALISEE (FORCES
!     ET MOMENTS PONCTUELS) SUR UNE BASE MODALE PERTURBEE PAR PRISE EN
!     COMPTE DU COUPLAGE FLUIDE STRUCTURE
!     APPELANT : OP0146 , OPERATEUR PROJ_SPEC_BASE
!-----------------------------------------------------------------------
! IN  : CASINT  : BOOLEEN, DONNE L'OPTION DE CALCUL
!       CASINT  = .TRUE.  => CALCUL DE TOUS LES INTERSPECTRES
!       CASINT  = .FALSE. => CALCUL DES AUTOSPECTRES UNIQUEMENT
! IN  : NOMU    : NOM UTILISATEUR
! IN  : SPECTR  : NOM DU CONCEPT SPECTRE
! IN  : BASE    : NOM DU CONCEPT MELASFLU
! IN  : VITE    : VITESSE ETUDIEE
! IN  : NUOR    : NUMEROS D'ORDRE DES MODES DU CONCEPT MELASFLU
! IN  : IMODI   : INDICE DU PREMIER MODE PRIS EN COMPTE
! IN  : IMODF   : INDICE DU DERNIER MODE PRIS EN COMPTE
! IN  : NBM     : NOMBRE DE MODES DU CONCEPT MELASFLU
! IN  : NBPF    : NOMBRE DE POINTS DE LA DISCRETISATION FREQUENTIELLE
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/axdipo.h"
#include "asterfort/deelpo.h"
#include "asterfort/exmano.h"
#include "asterfort/fointc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/scalep.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    logical :: casint
    integer :: imodi, imodf, nbm, nuor(nbm), nbpf, ij, nbval
    character(len=8) :: nomu
    character(len=19) :: spectr, base
    real(kind=8) :: vite
!
    integer :: ibid, dim, ival(2)
    real(kind=8) :: r8b, module
    real(kind=8) :: coefac(8), coefae(8), coefdc(6), coefde(6)
    complex(kind=8) :: c16b
    logical :: ltable
    character(len=8) :: k8b, caelem, modele, table, noma, nomno0
    character(len=16) :: config, nopart(2)
    character(len=19) :: typflu, nomfon
    character(len=24) :: spvain, spvate, spvare, spnnoe
    character(len=24) :: chvale
    character(len=24) :: remf, fsic, chrefe, mlgnno, mlgnma
!
!-----------------------------------------------------------------------
    integer :: iaxe, ichref, ideb, idec, ier, iex, iex1
    integer :: iex2, ifsic, iinte, il, im1, im1b, im2
    integer :: im2b, imail, inat, iremf, iret, iscal, ispin
    integer :: ispno, ispre, ispte, itypfl, iv, ivale, lwr
    integer :: nbexcp, nbma, nbmano, nbmr, numno0
    real(kind=8) :: beta, coedim, coef1, coef2, coefd, difphi, fr
    real(kind=8) :: frc, frref, phi1, phi2, phie, ptf, resuim
    real(kind=8) :: resure, rhof, s0, scal11, scal12, scal21, scal22
    real(kind=8) :: sref, tolr, uabs
!-----------------------------------------------------------------------
    data nopart / 'NUME_ORDRE_I' , 'NUME_ORDRE_J' /
!
    data coefac / 1.d-4  , 1.9d-1 , 7.d-2  , 1.6d0  ,&
     &              2.7d-5 , 1.9d-1 , 7.d-2  , 2.1d0  /
!
    data coefae / 1.d-5  , 5.d-1  , 2.d-2  , 2.9d0  ,&
     &              3.3d-4 , 1.d-1  , 2.d-2  , 2.8d0  /
!
    data coefdc / 4.d-5  , 1.9d-1 , 1.6d0  ,&
     &              2.7d-5 , 1.9d-1 , 2.1d0  /
!
    data coefde / 1.7d-5  , 2.2d-1 , 2.9d0  ,&
     &              1.7d-5  , 1.9d-1 , 2.8d0  /
!
!-----------------------------------------------------------------------
    call jemarq()
!
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
        call u2mess('F', 'MODELISA7_4')
    endif
!
!
! --- 2.RECUPERATION DU NOM DU CONCEPT MAILLAGE ---
!
    iv = 1
    write(chrefe,'(A8,A5,2I3.3,A5)') base(1:8),'.C01.',nuor(1),iv,&
     &                                 '.REFE'
    call jeveuo(chrefe, 'L', ichref)
    noma = zk24(ichref)(1:8)
!
!
! --- 3.RECUPERATION DES INFORMATIONS CARACTERISTIQUES DU SPECTRE ---
!
    spvain = spectr//'.VAIN'
    spvate = spectr//'.VATE'
    spvare = spectr//'.VARE'
    spnnoe = spectr//'.NNOE'
!
    call jeveuo(spvain, 'L', ispin)
    ltable = .false.
    if (zi(ispin+1) .eq. 0) ltable = .true.
!
    call jeveuo(spvate, 'L', ispte)
    caelem = zk16(ispte+1)(1:8)
    modele = zk16(ispte+2)(1:8)
!
    if (ltable) then
!
        table = zk16(ispte+3)(1:8)
        call tbliva(table, 0, k8b, ibid, r8b,&
                    c16b, k8b, k8b, r8b, 'DIMENSION',&
                    k8b, nbexcp, r8b, c16b, k8b,&
                    iret)
        if (iret .ne. 0) call u2mess('F', 'MODELISA2_89')
!
    else
!
        nbexcp = 2
        config = zk16(ispte+4)
        call jeveuo(spvare, 'L', ispre)
        rhof = zr(ispre)
        call jeveuo(spnnoe, 'L', ispno)
        nomno0 = zk8(ispno)
!
!-------RECUPERATION DU DIAMETRE EXTERIEUR DE LA POUTRE, NECESSAIRE AU
!       DIMENSIONNEMENT DE L'EXCITATION GRAPPE2
!
        mlgnno = noma//'.NOMNOE'
        call jenonu(jexnom(mlgnno, nomno0), numno0)
        mlgnma = noma//'.NOMMAI'
        call jelira(mlgnma, 'NOMMAX', nbma, k8b)
        call wkvect('&&SPECEP.TEMP.MAIL', 'V V I', nbma, imail)
        call exmano(noma, numno0, zi(imail), nbmano)
        if (nbmano .ne. 2) call u2mess('F', 'ALGELINE_70')
        call deelpo(caelem, noma, zi(imail), phi1)
        call deelpo(caelem, noma, zi(imail+1), phi2)
        tolr = r8prem()
        difphi = dble(abs(phi1-phi2))
        if (difphi .gt. phi1*tolr) then
            call u2mess('F', 'ALGELINE_71')
        else
            phie = phi1
        endif
!
!-------CALCUL DE COEFFICIENTS DE DIMENSIONNEMENT
!
        coef1 = 282.d0/890.d0
        coef2 = 0.77d0*0.77d0/4.d0
        coefd = phie/8.9d-2
!
    endif
!
!
! --- 4.DETERMINATION DE L'AXE DIRECTEUR DE LA POUTRE ---
!
    call axdipo(noma, caelem, modele, iaxe)
!
!
! --- 5.CALCUL DES PRODUITS SCALAIRES PHII(XK).NK ET PHII'(XM).NM ---
! ---   XK POINTS D'APPLICATION DES FORCES PONCTUELLES            ---
! ---   XM POINTS D'APPLICATION DES MOMENTS PONCTUELS             ---
! ---   NK ET NM DIRECTIONS D'APPLICATION DES EXCITATIONS         ---
!
    nbmr = imodf - imodi + 1
    call wkvect('&&SPECEP.TEMP.SCAL', 'V V R', nbexcp*nbmr, iscal)
    call scalep(spectr, noma, base, nuor, nbm,&
                imodi, nbmr, nbexcp, ltable, iaxe,&
                zr(iscal))
!
!
! --- 6.CALCUL DES INTERSPECTRES D'EXCITATIONS MODALES ---
! ---   BOUCLE SUR LE NOMBRE DE VITESSES               ---
!
    call wkvect('&&SPECEP.TEMP.SWR ', 'V V R', nbpf, lwr)
    dim = nbexcp*(nbexcp+1)/2
    dim = 2*nbpf*dim
    call wkvect('&&SPECEP.TEMP.INTE', 'V V R', dim, iinte)
!
! --- 6.1.RECUPERATION DE LA DISCRETISATION FREQUENTIELLE
    call jeveuo(nomu//'.FREQ', 'L', lwr)
!
! --- 6.2.INTERPOLATION DES INTERSPECTRES A PROJETER
!
    if (ltable) then
!
        do 20 iex2 = 1, nbexcp
            ival(2) = iex2
            do 21 iex1 = 1, iex2
                iex = iex2*(iex2-1)/2 + iex1
                ival(1) = iex1
                call tbliva(table, 2, nopart, ival, r8b,&
                            c16b, k8b, k8b, r8b, 'FONCTION_C',&
                            k8b, ibid, r8b, c16b, nomfon,&
                            iret)
                call assert(iret.eq.0)
                k8b = ' '
                do 22 il = 1, nbpf
                    ptf = zr(lwr+il-1)
                    call fointc('E', nomfon, 0, k8b, ptf,&
                                resure, resuim, ier)
                    if (ier .ne. 0) then
                        call u2mess('F', 'MODELISA7_5')
                    endif
                    idec = 2*nbpf*(iex-1)+2*(il-1)
                    zr(iinte+idec) = resure
                    zr(iinte+idec+1) = resuim
22              continue
21          continue
20      continue
!
    else if (config(1:7).eq.'ASC_CEN') then
!
        uabs = dble(abs(vite))
!
        do 30 iex2 = 1, nbexcp
            sref = coefac(4*(iex2-1)+1)
            frref = coefac(4*(iex2-1)+2)
            frc = coefac(4*(iex2-1)+3)
            beta = coefac(4*(iex2-1)+4)
            s0 = sref * ( 1.d0 + (frref/frc)**(beta) )
            inat = iex2 - int(iex2/2) * 2
            coedim = coef1 * coefd * coefd
            if (inat .eq. 0) coedim = coedim * coefd * coefd * coef2
            iex = iex2*(iex2+1)/2
            do 31 il = 1, nbpf
                idec = 2*nbpf*(iex-1)+2*(il-1)
                fr = zr(lwr+il-1)*phie/uabs
                module = 1.d0 + (fr/frc)**(beta)
                module = s0/module
                zr(iinte+idec) = coedim * module
31          continue
30      continue
!
    else if (config(1:7).eq.'ASC_EXC') then
!
        uabs = dble(abs(vite))
!
        do 40 iex2 = 1, nbexcp
            sref = coefae(4*(iex2-1)+1)
            frref = coefae(4*(iex2-1)+2)
            frc = coefae(4*(iex2-1)+3)
            beta = coefae(4*(iex2-1)+4)
            s0 = sref * ( 1.d0 + (frref/frc)**(beta) )
            inat = iex2 - int(iex2/2) * 2
            coedim = coef1 * coefd * coefd
            if (inat .eq. 0) coedim = coedim * coefd * coefd * coef2
            iex = iex2*(iex2+1)/2
            do 41 il = 1, nbpf
                idec = 2*nbpf*(iex-1)+2*(il-1)
                fr = zr(lwr+il-1)*phie/uabs
                module = 1.d0 + (fr/frc)**(beta)
                module = s0/module
                zr(iinte+idec) = coedim * module
41          continue
40      continue
!
    else if (config(1:7).eq.'DES_CEN') then
!
        uabs = dble(abs(vite))
!
        do 50 iex2 = 1, nbexcp
            s0 = coefdc(3*(iex2-1)+1)
            frc = coefdc(3*(iex2-1)+2)
            beta = coefdc(3*(iex2-1)+3)
            inat = iex2 - int(iex2/2) * 2
            coedim = coef1 * coefd * coefd
            if (inat .eq. 0) coedim = coedim * coefd * coefd * coef2
            iex = iex2*(iex2+1)/2
            do 51 il = 1, nbpf
                idec = 2*nbpf*(iex-1)+2*(il-1)
                fr = zr(lwr+il-1)*phie/uabs
                module = 1.d0 + (fr/frc)**(beta)
                module = s0/module
                zr(iinte+idec) = coedim * module
51          continue
50      continue
!
    else if (config(1:7).eq.'DES_EXC') then
!
        uabs = dble(abs(vite))
!
        do 60 iex2 = 1, nbexcp
            s0 = coefde(3*(iex2-1)+1)
            frc = coefde(3*(iex2-1)+2)
            beta = coefde(3*(iex2-1)+3)
            inat = iex2 - int(iex2/2) * 2
            coedim = coef1 * coefd * coefd
            if (inat .eq. 0) coedim = coedim * coefd * coefd * coef2
            iex = iex2*(iex2+1)/2
            do 61 il = 1, nbpf
                idec = 2*nbpf*(iex-1)+2*(il-1)
                fr = zr(lwr+il-1)*phie/uabs
                module = 1.d0 + (fr/frc)**(beta)
                module = s0/module
                zr(iinte+idec) = coedim * module
61          continue
60      continue
!
    endif
!
! --- 6.3.PROJECTION DES INTERSPECTRES
!
    ij = 0
    chvale = nomu//'.VALE'
    do 70 im2 = imodi, imodf
        ideb = im2
        if (casint) ideb = imodi
        do 71 im1 = ideb, im2
            ij = ij + 1
            call jeveuo(jexnum(chvale, ij), 'E', ivale)
            call jelira(jexnum(chvale, ij), 'LONMAX', nbval, k8b)
!
            im2b = im2 - imodi + 1
            im1b = im1 - imodi + 1
!
            if (ltable) then
!
                do 80 il = 1, nbpf
!
                    do 81 iex2 = 1, nbexcp
                        scal12 = zr(iscal+nbexcp*(im1b-1)+iex2-1)
                        scal22 = zr(iscal+nbexcp*(im2b-1)+iex2-1)
                        iex = iex2*(iex2+1)/2
                        idec = 2*nbpf*(iex-1)+2*(il-1)
                        if (nbval .eq. nbpf) then
                            zr(ivale+il-1) = zr(ivale+il-1) + scal12* scal22 * zr(iinte+idec)
                        else
                            zr(ivale+2*(il-1)) = zr(&
                                                 ivale+2*(il-1)) + scal12*scal22 * zr(iinte+idec)
                        endif
81                  continue
!
                    if (nbexcp .gt. 1) then
                        do 82 iex2 = 2, nbexcp
                            scal12 = zr(iscal+nbexcp*(im1b-1)+iex2-1)
                            scal22 = zr(iscal+nbexcp*(im2b-1)+iex2-1)
                            do 83 iex1 = 1, iex2-1
                                scal11 = zr(iscal+nbexcp*(im1b-1)+ iex1-1)
                                scal21 = zr(iscal+nbexcp*(im2b-1)+ iex1-1)
                                iex = iex2*(iex2-1)/2 + iex1
                                idec = 2*nbpf*(iex-1)+2*(il-1)
                                if (nbval .eq. nbpf) then
                                    zr(ivale+il-1) = zr(ivale+il-1) + ( scal11*scal22 + scal12*sc&
                                                     &al21 ) * zr(iinte+idec)
                                else
                                    zr(ivale+2*(il-1)) = zr(&
                                                         ivale+2*( il-1)) + ( scal11*scal22 + sca&
                                                         &l12* scal21 ) * zr(iinte+idec&
                                                         )
                                    zr(ivale+2*(il-1)+1) = zr(&
                                                           ivale+2* (il-1)+1) + ( scal11*scal22 -&
                                                           & scal12*scal21 ) * zr(iinte+idec+ 1&
                                                           )
                                endif
83                          continue
82                      continue
                    endif
!
80              continue
!
            else
!
                coedim = 0.25d0 * coef1*coef1 * rhof*rhof * uabs*uabs* uabs * phie*phie*phie
                do 90 il = 1, nbpf
                    do 91 iex2 = 1, nbexcp
                        scal12 = zr(iscal+nbexcp*(im1b-1)+iex2-1)
                        scal22 = zr(iscal+nbexcp*(im2b-1)+iex2-1)
                        iex = iex2*(iex2+1)/2
                        idec = 2*nbpf*(iex-1)+2*(il-1)
                        if (nbval .eq. nbpf) then
                            zr(ivale+il-1) = zr(ivale+il-1) + coedim * scal12*scal22 * zr(iinte+i&
                                             &dec)
                        else
                            zr(ivale+2*(il-1)) = zr(&
                                                 ivale+2*(il-1)&
                                                 ) + coedim * scal12*scal22 * zr(iinte+idec&
                                                 )
                        endif
91                  continue
90              continue
!
            endif
71      continue
70  continue
!
    call jedetr('&&SPECEP.TEMP.MAIL')
    call jedetr('&&SPECEP.TEMP.SCAL')
    call jedetr('&&SPECEP.TEMP.SWR ')
    call jedetr('&&SPECEP.TEMP.INTE')
!
    call jedema()
end subroutine
