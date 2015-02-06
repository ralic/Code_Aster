subroutine te0579(option, nomte)
    implicit none
!
! ======================================================================
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN FLUX REPARTI SUR LES BORDS
!          D'UN MODELE COUPLE HM-XFEM
!
!          OPTION : 'CHAR_MECA_FLUX_R' ET 'CHAR_MECA_FLUX_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
#include "asterf_types.h"
#include "asterfort/abscvf.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/dfdm2b.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/iselli.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/normev.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xhmddl.h"
#include "asterfort/xlinhm.h"
#include "blas/ddot.h"
#include "jeveux.h"
!
    character(len=8) :: noma, elrefp, enr, elref
    character(len=16) :: nomte, option
    integer :: jpintt, jcnset, jheavt, jlonch, kk
    integer :: jpmilt, nfiss, ifiss, jtab(7), ncomp
    integer :: ier, ndim, nno, nnop, nnops, npg, nnos, kpg
    integer :: ipoids, ivf, idfde, igeom, ires, i, j
    integer :: nfh, nse, ise, iret, pos, ndime, nddl
    integer :: in, ino, iadzi, iazk24, jstno, itemps, jlsn, jheavn, ncompn, jheavs
    integer :: iflux, idec, nddls, nddlm, nnopm, ifluxf
    real(kind=8) :: ff(27), dfdi(27,3)
    real(kind=8) :: rb1(3), rb2, nbid(3), rb3, rb4
    real(kind=8) :: poids, valpar(4), xg(3)
    real(kind=8) :: deltat, coorse(81), flux, tplus
    character(len=8) :: nompar(4), elrefl
    aster_logical :: pre1
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     ELEMENT DE REFERENCE PARENT
    call elref1(elrefp)
    call elrefe_info(fami='RIGI',ndim=ndime,nno=nnop,nnos=nnops)
!
    ASSERT(ndime.eq.1.or.ndime.eq.2)
!
!     DIMENSION DE L'ESPACE
    call tecael(iadzi, iazk24)
    noma=zk24(iazk24)(1:8)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
!     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
!     NDIM EST LA DIMENSION DU MAILLAGE
!     NDIME EST DIMENSION DE L'ELEMENT FINI
!     
!     SOUS-ELEMENT DE REFERENCE EN HM-XFEM
    if (ndime.eq.1) then
       elref='SE3'
    else if (ndime.eq.2) then
       elref='TR6'
    endif
!
    call vecini(3,0.d0, xg)
    call vecini(3,0.d0, nbid)
    poids = 0.d0
    rb2 = 0.d0
    rb3 = 0.d0
    rb4 = 0.d0
!     RECUPERATION DES CARACTERISTIQUES DES SOUS-ELEMENTS DE REF
    call elrefe_info(elrefe=elref,fami='RIGI',nno=nno,nnos=nnos,&
                     npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde)
!
    pre1=.false.
!
!     ON NE CONSIDERE QUE LES ELEMENTS HM_ POUR LE CALCUL DES FLUX
!     AU NIVEAU DES ELEMENTS DE BORDS
    if (nomte(1:2) .eq. 'HM') then
        pre1=.true.
    else
        ASSERT(.false.)
    endif
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
!     IL NE FAUT PAS APPELER XTEINI CAR IL NE GERE PAS LES ELEMENTS
!     DE BORD
!
    nfh = 0
    nfiss = 1
    ifiss = 1
!
    call teattr('S', 'XFEM', enr, ier)
!
    if (enr(1:2) .eq. 'XH') then
!     NOMBRE DE FISSURES
        call tecach('NOO', 'PHEAVTO', 'L', iret, nval=7,&
                itab=jtab)
        ncomp = jtab(2)
        nfiss = jtab(7)
        nfh = 1
        call jevech('PHEA_NO', 'L', jheavn)
        call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
        ncompn = jtab(2)/jtab(3)
        call jevech('PHEA_SE', 'L', jheavs)
    endif
!
    ASSERT(nfh.gt.0)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
!
!     SI LE FLUX N'EST CONNU SUR AUCUN NOEUD, ON LE PREND=0.
!
    if (pre1 .and. (option.eq.'CHAR_MECA_FLUX_R')) then
        call jevecd('PFLUXR', iflux, 0.d0)
        call jevech('PTEMPSR', 'L', itemps)
        deltat = zr(itemps+1)
    else if (pre1 .and. (option.eq.'CHAR_MECA_FLUX_F')) then
        call jevech('PFLUXF', 'L', ifluxf)
        call jevech('PTEMPSR', 'L', itemps)
        tplus = zr(itemps)
        deltat = zr(itemps+1)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        if (ndime.eq.1) then
           nompar(3) = 'INST'
           valpar(3) = tplus
        else if (ndime.eq.2) then
           nompar(3) = 'Z'
           nompar(4) = 'INST'
           valpar(4) = tplus
        endif
    else
        ASSERT(.false.)
    endif
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PSTANO', 'L', jstno)
    call jevech('PLSN', 'L', jlsn)
!
!     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr('S', 'XFEM', enr, ier)
!
    if (ier.eq.0.and.(enr.eq.'XH').and.(.not.iselli(elref))) call jevech('PPMILTO','L', jpmilt)
!
    call jevech('PVECTUR', 'E', ires)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMEN
    nse=zi(jlonch-1+1)
!
!     BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES NOEUDS DU SOUS-SEGMENT
        do 111 in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do 112 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
112         continue
111     continue
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
        do 200 kpg = 1, npg
!
!         CALCUL DU POIDS : POIDS = POIDS DE GAUSS * DET(J)
            if (ndime .eq. 1) then
                kk = (kpg-1)*nno
                call dfdm1d(nno, zr(ipoids-1+kpg), zr(idfde+kk), coorse, rb1,&
                            rb2, poids, rb3, rb4)
            else if (ndime.eq.2) then
                 kk = 2*(kpg-1)*nno
                 call dfdm2b(nno, zr(ipoids-1+kpg), zr(idfde+kk), coorse,&
                             poids, nbid)
            endif
!
!         CALCUL DES FONCTIONS DE FORME AU POINT DE GAUSS DANS L'ELEMENT PARENT LINEAIRE
            call vecini(3,0.d0,xg)
            do i = 1, ndim
                do ino = 1, nno
                   xg(i) = xg(i) + coorse(ndim*(ino-1)+i)*zr(ivf-1+(kpg-1)*nno+ino)
                end do
            end do
!
            call xlinhm(elrefp, elrefl)
!
            call reeref(elrefl, nnops, zr(igeom), xg, ndim, nbid, ff, dfdi)
!
!         CALCUL DES FORCES REPARTIES SUIVANT LES OPTIONS
!         -----------------------------------------------
!
            if (pre1 .and. (option.eq.'CHAR_MECA_FLUX_R')) then
!
!           DECALAGE POUR LE CALCUL DES FLUX AU POINT DE GAUSS KPG
!           DU SOUS-ELEMENT COURANT ISE
!
                idec=npg*(ise-1)+kpg
!
                flux = 0.d0
                do ino = 1, nnops
                   flux = flux + zr(iflux-1+ino)*ff(ino)
                end do
!
!           CALCUL EFFECTIF DU SECOND MEMBRE (ATTENTION CALCUL EN
!           REGIME TRANSITOIRE => DELTAT)
!           --------------------------------
                pos=0
                do 290 ino = 1, nnops
!
!             ON ZAPPE LES TERMES MECANIQUES CLASSIQUES POUR TOMBER
!             DIRECTEMENT SUR LES LIGNES CORRESPONDANT A PRE1
                    pos=pos+(ndim+1)
!
!             TERME CLASSIQUE (=> PRE1)
                    zr(ires-1+pos) = zr(ires-1+pos)-flux*poids*deltat* ff(ino)
!
!             ON ZAPPE LES TERMES MECANIQUES HEAVISIDE POUR TOMBER
!             DIRECTEMENT SUR LES LIGNES CORRESPONDANT A HPRE1
                    pos=pos+(ndim+1)
!
!             TERME HEAVISIDE
                    zr(ires-1+pos) = zr(ires-1+pos) - xcalc_heav(&
                                                           zi(jheavn-1+ncompn*(ino-1)+1),&
                                                           zi(jheavs-1+ise),&
                                                           zi(jheavn-1+ncompn*(ino-1)+ncompn))*&
                                                      deltat*flux*poids*ff(ino)
!
290             continue
            else if (pre1 .and. (option.eq.'CHAR_MECA_FLUX_F')) then
               if (ndime.eq.1) then
                    kk = (kpg-1)*nno
                    valpar(1) = xg(1)
                    valpar(2) = xg(2)
                    call fointe('FM', zk8(ifluxf+0), 3, nompar, valpar,&
                                flux, iret)
               else if (ndime.eq.2) then
                    kk = (kpg-1)*nno
                    valpar(1) = xg(1)
                    valpar(2) = xg(2)
                    valpar(3) = xg(3)
                    call fointe('FM', zk8(ifluxf+0), 4, nompar, valpar,&
                                flux, iret)
               endif
               pos=0
               do 80 ino = 1, nnops
!             ON ZAPPE LES TERMES MECANIQUES CLASSIQUES POUR TOMBER
!             DIRECTEMENT SUR LES LIGNES CORRESPONDANT A PRE1
                    pos=pos+(ndim+1)
!
!             TERME CLASSIQUE (=> PRE1)
                    zr(ires-1+pos) = zr(ires-1+pos)-flux*poids*deltat* ff(ino)
!
!             ON ZAPPE LES TERMES MECANIQUES HEAVISIDE POUR TOMBER
!             DIRECTEMENT SUR LES LIGNES CORRESPONDANT A HPRE1
                    pos=pos+(ndim+1)
!
!             TERME HEAVISIDE
                    zr(ires-1+pos) = zr(ires-1+pos) - xcalc_heav(&
                                                           zi(jheavn-1+ncompn*(ino-1)+1),&
                                                           zi(jheavs-1+ise),&
                                                           zi(jheavn-1+ncompn*(ino-1)+ncompn))*&
                                                      deltat*flux*&
                                                      poids*ff(ino)
80              continue
            else
                call utmess('F', 'XFEM_15')
            endif
200     continue
!
!-----------------------------------------------------------------------
!         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
110 continue
!
!     SUPPRESSION DES DDLS SUPERFLUS
    nddls = ndim*(1+nfh) + (1+nfh)
    nddlm = ndim*(1+nfh)
    nnopm = nnop - nnops
    nddl = nnops*nddls + nnopm*nddlm
!
    call xhmddl(ndim, nddls, nddl, nnop, nnops,&
                zi(jstno), .false._1, option, nomte, rb1,&
                zr(ires), nddlm)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
!
end subroutine
