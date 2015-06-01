subroutine spect1(casint, nomu, spectr, ispect, base,&
                  vite, nuor, imodi, imodf, nbm,&
                  nbpf, nomzon, vmoyzi, vmoyto)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!     PROJECTION D UN SPECTRE DE TURBULENCE DE TYPE "LONGUEUR DE
!     CORRELATION" SUR UNE BASE MODALE PERTURBEE PAR PRISE EN COMPTE
!     DU COUPLAGE FLUIDE STRUCTURE
!     APPELANT : OP0146 , OPERATEUR PROJ_SPEC_BASE
!-----------------------------------------------------------------------
! IN  : CASINT  : BOOLEEN, DONNE L'OPTION DE CALCUL
!       CASINT  = .TRUE.  => CALCUL DE TOUS LES INTERSPECTRES
!       CASINT  = .FALSE. => CALCUL DES AUTOSPECTRES UNIQUEMENT
! IN  : NOMU    : NOM UTILISATEUR
! IN  : SPECTR  : NOM DU CONCEPT SPECTRE
! IN  : ISPECT  : NUMERO DU SPECTRE
! IN  : BASE    : NOM DU CONCEPT MELASFLU
! IN  : VITE    : VITESSES ETUDIEES
! IN  : NUOR    : NUMEROS D'ORDRE DES MODES DU CONCEPT MELASFLU
! IN  : IMODI   : INDICE DU PREMIER MODE PRIS EN COMPTE
! IN  : IMODF   : INDICE DU DERNIER MODE PRIS EN COMPTE
! IN  : NBM     : NOMBRE DE MODES DU CONCEPT MELASFLU
! IN  : NBPF    : NOMBRE DE POINTS DE LA DISCRETISATION FREQUENTIELLE
! IN  : NOMZON  : NOM DE LA ZONE (OU PROFI DE VITESSE) ASSOCIEE AU
!                 SPECTRE COURANT
! IN  : VMOYZI  : VITESSE MOYENNE DE LA ZONE NOMZON
! IN  : VMOYTO  : VITESSE MOYENNE DE L ENSEMBLE DES ZONES D EXCITATION
!                 DU FLUIDE
!-----------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/coesp1.h"
#include "asterfort/coesp4.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/permnoe.h"
#include "asterfort/recude.h"
#include "asterfort/spect2.h"
#include "asterfort/spect4.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    aster_logical :: casint
    character(len=8) :: nomu, nomzon
    character(len=19) :: spectr, base
    real(kind=8) :: vite, vmoyzi, vmoyto
    integer :: nbm, nuor(nbm), vali(2)
!
    integer :: dim, nbval, icmp
    character(len=8) :: nomcmp, depla(3), maillage
    character(len=19) :: typflu, caelem
    character(len=24) :: refe, fsic, fsvi, fsvk, profvn, frhoe, nomcha, valr
    character(len=24) :: chvale
    character(len=3) :: tout
    real(kind=8) :: rbid, valx(3)
    integer :: ij
!-----------------------------------------------------------------------
    integer :: i, ic, icha, ide, ideb, idefm, idep
    integer :: ier, ifre, ifsic, ifsvi, ifsvk, ii, ik
    integer :: il, ilc2, im, im1, im1b, im2, im2b
    integer :: imb, imodf, imodi, ip, ipvn, irefe, irhoe
    integer :: irsp, ispect, itypfl, ivale, ivitn, iz
    integer :: jm, jmb, kk, lwr, n1, n2, nbcmp
    integer :: nbfonc, nbp, nbpf, nzex
    real(kind=8) :: beta, beta1, beta2, eps, err, fr, frc
    real(kind=8) :: gamma, phi0, phi01, phi02, phie, r1, ren
    real(kind=8) :: rom, rov, sx, tauxv, tol, vitezi, x1
    real(kind=8) :: x2, xlc, xnu
!-----------------------------------------------------------------------
    data depla   /'DX      ','DY      ','DZ      '/
!-----------------------------------------------------------------------
    call jemarq()
    tol = 1.d-05
    ier = 0
!
!
! --- 1.RECUPERATION D'INFORMATIONS PAR INDIRECTION ---
!
! --- 1.1.NOM DU CONCEPT TYPE_FLUI_STRU ASSOCIE A L'ETUDE
!
    refe = base//'.REMF'
    call jeveuo(refe, 'L', irefe)
    typflu = zk8(irefe)
    call dismoi('NOM_MAILLA', zk8(irefe+1), 'RESULTAT', repk=maillage)
!
! --- 1.2.TEST DE COMPATIBILITE TYPE DE SPECTRE/CONFIGURATION ETUDIEE
!
    fsic = typflu//'.FSIC'
    call jeveuo(fsic, 'L', ifsic)
    itypfl = zi(ifsic)
    if (itypfl .ne. 1) then
        call utmess('F', 'MODELISA7_4')
    endif
!
! --- 1.3.RECUPERATION DE NOMS DE CONCEPTS PAR INDIRECTION
!
    fsvi = typflu//'.FSVI'
    call jeveuo(fsvi, 'L', ifsvi)
    nzex = zi(ifsvi+1)
    fsvk = typflu//'.FSVK'
    call jeveuo(fsvk, 'L', ifsvk)
    caelem = zk8(ifsvk)
    nomcmp = zk8(ifsvk+1)
    frhoe = zk8(ifsvk+3)
!
    do 10 iz = 1, nzex
        if (nomzon .eq. zk8(ifsvk+3+iz)) then
            profvn = nomzon
            goto 11
        endif
 10 end do
 11 continue
!
!
! --- 2.ACCES AU PROFIL DE VITESSE
! ---         AU PROFIL DE MASSE VOLUMIQUE DU FLUIDE EXTERNE ---
!
! --- 2.1.ACCES AUX OBJETS .VALE
!
    profvn = profvn(1:19)//'.VALE'
    call jeveuo(profvn, 'L', ipvn)
    frhoe = frhoe(1:19)//'.VALE'
    call jeveuo(frhoe, 'L', irhoe)
!
! --- 2.2.RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
!
    call jelira(profvn, 'LONUTI', nbp)
    nbp = nbp / 2
!
!
! --- 3.RECUPERATION DU DIAMETRE EXTERIEUR DU TUBE ---
!
    call recude(caelem, phie, rbid)
!
!
! --- 4.RECUPERATION DE LA LONGUEUR DE CORRELATION PHYSIQUE ---
!
    valr = spectr//'.VARE'
    call jeveuo(valr, 'L', irsp)
!
    xlc = zr(irsp)
    xlc = xlc*phie
!
!
! --- 5.CALCUL DES LONGUEURS DE CORRELATION GENERALISEES ---
!
! --- 5.1.RECHERCHE POUR L INTEGRALE DOUBLE DES BORNES
! ---     DE LA ZONE OU LA FONCTION EST NON NULLE
!
    x1 = 0.d0
    x2 = 0.d0
    do 50 ik = 1, nbp
        if (zr(ipvn+nbp+ik-1) .ne. 0.d0) then
            x1 = zr(ipvn+ik-1)
            n1 = ik
            goto 51
        endif
 50 end do
 51 continue
!
    do 60 ik = nbp, 1, -1
        if (zr(ipvn+nbp+ik-1) .ne. 0.d0) then
            x2 = zr(ipvn+ik-1)
            n2 = ik
            goto 61
        endif
 60 end do
 61 continue
!
! --- 5.2.CREATION D UN PROFIL DE VITESSE NORMALISE POUR
! ---     LE CALCUL DES LONGUEURS DE CORRELATION GENERALISEES
!
    call wkvect('&&SPECT1.TEMP.VITN', 'V V R', nbp*2, ivitn)
    do 70 i = n1, n2
        zr(ivitn+i-1+nbp) = zr(ipvn+i-1+nbp) / vmoyzi
 70 end do
    do 71 i = 1, nbp
        zr(ivitn+i-1 ) = zr(ipvn+i-1 )
 71 end do
!
! --- 5.3.CREATION D UN VECTEUR DE TRAVAIL POUR STOCKER
! ---     LES LONGUEURS DE CORRELATION GENERALISEES
!
    dim = (imodf-imodi)+1
    nbfonc = (dim* (dim+1))/2
    call wkvect('&&SPECT1.TEMP.LC2', 'V V R', nbfonc, ilc2)
!
! --- 5.4 CREATION ET REMPLISSAGE DU VECTEUR DE TRAVAIL .DEFM ---
!     (DEFORMEE POUR CHAQUE MODE, EN CHAQUE NOEUD, DANS LA DIRECTION
!      CHOISIE PAR L'UTILISATEUR OU PRISE EN COMPTE DE TOUTES
!      LES DIRECTIONS)
!
    call wkvect('&&SPECT1.TEMP.DEFM', 'V V R', nbp*nbm, idefm)
!
    call getvtx(' ', 'TOUT_CMP', scal=tout, nbret=nbval)
    if (tout .eq. 'NON') then
        nbcmp = 1
        do 20 ide = 1, 3
            if (depla(ide) .eq. nomcmp) then
                idep = ide
            endif
 20     continue
    else
        nbcmp=3
    endif
!
    do 105 icmp = 1, nbcmp
        nomcha(1:13) = base(1:8)//'.C01.'
        nomcha(17:24) = '001.VALE'
        if (tout .eq. 'NON') then
            do 40 im = 1, nbm
                write(nomcha(14:16),'(I3.3)') nuor(im)
                call jeveuo(nomcha, 'L', icha)
                do 30 ip = 1, nbp
                    zr(idefm+nbp*(im-1)+ip-1) = zr(icha+6*(ip-1)+idep- 1)
 30             continue
                call permnoe(maillage, zr(idefm+nbp*(im-1)), 1, nbp)
                call jelibe(nomcha)
 40         continue
        else
            do 45 im = 1, nbm
                write(nomcha(14:16),'(I3.3)') nuor(im)
                call jeveuo(nomcha, 'L', icha)              
                do 35 ip = 1, nbp
                    zr(idefm+nbp*(im-1)+ip-1) = zr(icha+6*(ip-1)+icmp- 1)
 35             continue
                call permnoe(maillage, zr(idefm+nbp*(im-1)), 1, nbp)
                call jelibe(nomcha)
 45         continue
        endif 
!
        do 90 jm = imodi, imodf
            ideb = jm
            if (casint) ideb = imodi
            do 80 im = ideb, jm
                jmb = jm - imodi + 1
                imb = im - imodi + 1
                kk = (jmb* (jmb-1))/2 + imb
                zr(ilc2+kk-1) = zr(ilc2+kk-1)+spect2(x1,x2,xlc,zr( ivitn), zr(irhoe),zr(idefm),sp&
                                &ect4,tol,ier, r1,err, nbp,im,jm)
!
                if (ier .ne. 0) then
                    vali(1)=nuor(jm)
                    vali(2)=nuor(im)
                    valx(1)=zr(ilc2+kk-1)
                    valx(2)=r1
                    valx(3)=err
                    call utmess('A', 'MODELISA7_7', ni=2, vali=vali, nr=3,&
                                valr=valx)
                endif
 80         continue
 90     continue
105 end do
!
!
! --- 6.CALCUL DES INTERSPECTRES D'EXCITATIONS MODALES ---
!
! --- 6.1.CREATION D UN VECTEUR DE TRAVAIL POUR STOCKER
! ---     LES VALEURS DU SPECTRE
!
    call wkvect('&&SPECT1.TEMP.SWR ', 'V V R', nbpf, lwr)
!
! --- 6.2.BOUCLE POUR CHAQUE VITESSE
!
    vitezi = vite*vmoyzi/vmoyto
!
! --- 6.2.1.RECUPERATION DE LA DISCRETISATION FREQUENTIELLE
    call jeveuo(nomu//'.DISC', 'L', ivale)
!
! --- 6.2.2.CALCUL DES VALEURS DU SPECTRE
!
    if (ispect .eq. 1) then
!
        xnu = zr(irsp+1)
        ren = (vitezi*phie)/xnu
        ren = dble(abs(ren))
!
        call coesp1(ren, phi0, eps, frc, beta)
!
        do 100 ifre = 1, nbpf
            fr = zr(ivale+ifre-1)
            fr = (fr*phie)/vitezi
            fr = dble(abs(fr))
            sx = (fr/frc)** (beta/2.d0)
            sx = (1.d0-sx)* (1.d0-sx) + 4.d0*eps*eps*sx
            zr(lwr+ifre-1) = phi0/sx
100     continue
!
    else if (ispect.eq.2) then
!
        frc = zr(irsp+1)
        phi0 = zr(irsp+2)
        beta = zr(irsp+3)
!
        do 110 ifre = 1, nbpf
            fr = zr(ivale+ifre-1)
            fr = (fr*phie)/vitezi
            fr = dble(abs(fr))
            sx = phi0/ (1.d0+ (fr/frc)**beta)
            zr(lwr+ifre-1) = sx
110     continue
!
    else if (ispect.eq.3) then
!
        do 120 ifre = 1, nbpf
            fr = zr(ivale+ifre-1)
            fr = (fr*phie)/vitezi
            fr = dble(abs(fr))
!
            frc = zr(irsp+1)
            phi01 = zr(irsp+2)
            beta1 = zr(irsp+3)
            phi02 = zr(irsp+4)
            beta2 = zr(irsp+5)
!
            if (fr .le. frc) then
                phi0 = phi01
                beta = beta1
            else
                phi0 = phi02
                beta = beta2
            endif
!
            sx = phi0/ (fr**beta)
            zr(lwr+ifre-1) = sx
120     continue
!
    else if (ispect.eq.4) then
!
        rom = 0.d0
        ic = 0
        do 130 ii = 1, nbp
            if (zr(ipvn+nbp+ii-1) .ne. 0.d0) then
                rom = rom + zr(irhoe+nbp+ii-1)
                ic = ic + 1
            endif
130     continue
!
        rom = rom/dble(ic)
        rov = rom*vitezi
        rov = dble(abs(rov))
        tauxv = zr(irsp+1)
        beta = zr(irsp+2)
        gamma = zr(irsp+3)
!
        call coesp4(tauxv, phi0)
!
        do 140 ifre = 1, nbpf
            fr = zr(ivale+ifre-1)
            fr = (fr*phie)/vitezi
            fr = dble(abs(fr))
            sx = phi0/ ((fr**beta)* (rov**gamma))
            zr(lwr+ifre-1) = sx
140     continue
!
    endif
!
! --- 6.2.3.CALCUL DES INTERSPECTRES
!
    chvale = nomu//'.VALE'
!
    ij = 0
    do 170 im2 = imodi, imodf
        ideb = im2
        if (casint) ideb = imodi
        do 160 im1 = ideb, im2
            ij = ij + 1
            call jeveuo(jexnum(chvale, ij), 'E', ivale)
            call jelira(jexnum(chvale, ij), 'LONMAX', nbval)
!
            im2b = im2 - imodi + 1
            im1b = im1 - imodi + 1
            kk = im2b* (im2b-1)/2 + im1b
!
            do 150 il = 1, nbpf
                if (nbval .eq. nbpf) then
                    zr(ivale+il-1) = zr(ivale+il-1) + 0.25d0*phie* phie*phie*vitezi* vitezi*dble(&
                                     &abs(vitezi))* zr(ilc2+kk-1)*zr(lwr+il-1)
                else
                    zr(ivale+2* (il-1)) = zr(&
                                          ivale+2* (il-1)) + 0.25d0*phie*phie*phie*vitezi* vitezi&
                                          &*dble(abs( vitezi))* zr(ilc2+kk-1)*zr(lwr+il-1&
                                          )
                    zr(ivale+2* (il-1)+1) = 0.d0
                endif
150         continue
160     continue
170 continue
!
    call jedetr('&&SPECT1.TEMP.DEFM')
    call jedetr('&&SPECT1.TEMP.VITN')
    call jedetr('&&SPECT1.TEMP.LC2')
    call jedetr('&&SPECT1.TEMP.SWR ')
!
    call jedema()
end subroutine
