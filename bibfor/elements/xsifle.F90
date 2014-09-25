subroutine xsifle(ndim, ifa, jptint, jaint, cface,&
                  igeom, nfh, singu, nfe, ddlc,&
                  ddlm, jlst, ipres, ipref, itemps,&
                  idepl, nnop, valres, basloc, ithet,&
                  nompar, presn, option, igthet, jbasec,&
                  contac)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1306,W1504
    implicit none
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/elelin.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterc/r8pi.h"
#include "asterfort/tecael.h"
#include "asterfort/xjacf2.h"
#include "asterfort/xjacff.h"
#include "asterfort/xsifl1.h"
#include "asterfort/xsifl2.h"
#include "asterfort/xxmmvd.h"
#include "jeveux.h"
!
    character(len=8) :: nompar(4)
    character(len=16) :: option
    integer :: ndim, ifa, cface(5, 3), jaint, igeom, nfh, singu, jlst, ipres
    integer :: nfe, ddlc, ipref, itemps, nnop, ithet, jptint, igthet, idepl
    integer :: ddlm, jbasec, contac
    real(kind=8) :: valres(3)
    real(kind=8) :: basloc(9*nnop), presn(27)
!
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE POST-TRAITEMENT
!                          EN MÉCANIQUE DE LA RUPTURE
!                          SUR LES LEVRES DES FISSURES X-FEM
!
! IN  NDIM  : DIMENSION DU PROBLEME
! IN  IFA   : NUMERO DE LA FACETTE DE CONTACT DS L ELEMENT PARENT
! IN JPTINT : ADRESSE TOPOFAC.PI - COORDONNEES PTS D'INTERSECTION
! IN JAINT  : ADRESSE TOPOFAC.AI - ARETES INTERSECTEES
! IN CFACE  : TABLEAU TOPOFAC.CF - CONNECTIVITE DES FACETTES
! IN IGEOM  : ADRESSE COORDONNEES NOEUDS MAILLE PARENT
! IN NFH    : NOMBRE ENRICHISSEMENT HEAVISIDE
! IN SINGU  : PRESENCE ENRICHISSEMENT CRACK TIP
! IN NFE    : NOMBRE ENRICHISSEMENTS CRACK TIP
! IN DDLC   : NOMBRE DDLS CONTACT SUR CHAQUE NOEUD QUI EN PORTE
! IN DDLM   : NOMBRE DDL SUR CHAQUE NOEUDS MILIEU
! IN JLST   : ADRESSE LEVEL-SET TANGENTE
! IN IPRES  : PRESSION SI CONSTANTE
! IN IPREF  : PRESSION SI FONCTION
! IN ITEMPS : CARTE (CONSTANTE) AVEC L INSTANT
!        UTILISE SI PRESSION FONCTION DU TEMPS
! IN IDEPL  : ADRESSE DEPLACEMENT
! IN NNOP   : NOMBRE DE NOEUDS MAILLE PARENT
! IN VALRES : CONSTANTES ELASTIQUES MATERIAU /E, NU, ALPHA/
! IN BASLOC : FISS.BASLOC -  BASE DU PROJETE AU NOEUD
! IN ITHET  : CHAMP THETA AUX NOEUDS DU MAILLAGE PARENT
! IN NOMPAR : NOM DES PARAMETRES SI PRESSION VARIABLE
! IN PRESN  : PRESSION AUX NOEUDS PARENT
! IN OPTION : OPTION DE CALCUL
! OUT IGTHET: G (OPTION CALC_G) ET K1, K2, K3 (SI OPTION CALC_K_G)
! IN JBASEC : ADRESSE TOPOFAC.BA
! IN CONTAC : TYPE DE CONTACT P1P1 ou P2P1
!
!
    integer :: i, nli, in(3), iadzi, iazk24, ibid2(12, 3), ibid, fac(6, 4), nbf
    integer :: ar(12, 3), nbar, cpt, ino, nnof, npgf, ipoidf, ivff, idfdef
    integer :: ipgf, zxain
    integer :: ddld, ddls, nnops
    real(kind=8) :: mult, xg(4), jac, ff(27), nd(3)
    real(kind=8) :: angl(2)
    real(kind=8) :: e, nu, mu, ka, coeff, coeff3, r27bid(27)
    real(kind=8) :: dfdi(nnop, ndim)
    real(kind=8) :: he(2)
!
    integer :: l, nnos, nno
    real(kind=8) :: ffc(27)
    character(len=8) :: elref, typma, fpg, elc, elrefc
    real(kind=8) :: tau1(3), tau2(3)
    data     he / -1.d0 , 1.d0/
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
!     PAR CONVENTION :
!     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
!     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
    angl(1) = -r8pi()
    angl(2) = r8pi()
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!
    call elref1(elref)
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
!
    if (ndim .eq. 3) then
        call confac(typma, ibid2, ibid, fac, nbf)
        elc='TR3'
        if(option.eq.'CALC_K_G_COHE') fpg='FPG4'
        if(option.ne.'CALC_K_G_COHE') fpg='XCON'
    else if (ndim.eq.2) then
        call conare(typma, ar, nbar)
        elc='SE2'
        fpg='MASS'
    endif
!
!     PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
    mult=1.d0
    do 101 i = 1, ndim
        nli=cface(ifa,i)
        in(i)=nint(zr(jaint-1+zxain*(nli-1)+2))
101  end do
!     SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
    if (ndim .eq. 3) then
        if (in(1) .ne. 0 .and. in(2) .ne. 0 .and. in(3) .ne. 0) then
            do 102 i = 1, nbf
                cpt=0
                do 103 ino = 1, 4
                    if (in(1) .eq. fac(i,ino) .or. in(2) .eq. fac(i,ino) .or. in(3) .eq.&
                        fac(i,ino)) cpt=cpt+1
103              continue
                if (cpt .eq. 3) then
                    mult=0.5d0
                    goto 104
                endif
102          continue
        endif
    else if (ndim .eq. 2) then
        if (in(1) .ne. 0 .and. in(2) .ne. 0) then
            do 1021 i = 1, nbar
                cpt=0
                do 1031 ino = 1, 2
                    if (in(1) .eq. ar(i,ino) .or. in(2) .eq. ar(i,ino)) cpt=cpt+1
1031              continue
                if (cpt .eq. 2) then
                    mult=0.5d0
                    goto 104
                endif
1021          continue
        endif
    endif
104  continue
!
    call elrefe_info(elrefe=elc,fami=fpg,nno=nnof,&
                     npg=npgf,jpoids=ipoidf,jvf=ivff,jdfde=idfdef)
!
!     MATÉRIAU HOMOGENE
!     ON PEUT PAS LE RECUPERER SUR LES POINTS DE GAUSS DES FACETTES
!     CAR LA FAMILLE CONCATENEE DES PG DES FACETTES N'EXISTE PAS
    e = valres(1)
    nu = valres(2)
    mu = e / (2.d0*(1.d0+nu))
!
!   DEFINITION DEFORMATIONS PLANES!
    ka = 3.d0 - 4.d0*nu
    coeff = e / (1.d0-nu*nu)
    coeff3 = 2.d0 * mu
!
!     ----------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
    do 900 ipgf = 1, npgf
!
!       CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
!       ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!       ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
!       ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
!       ET DE DFDI : DERIVVES DES FF PARENT
        if (ndim .eq. 3) then
            call elelin(contac, elref, elrefc, nno, nnos)
            nnops = nnos
            ASSERT(nno.eq.nnop)
            elrefc='NON'
            call xjacff(elref, elrefc, elc, ndim, fpg,&
                        jptint, ifa, cface, ipgf, nnop,&
                        igeom, jbasec, xg, jac, ff,&
                        ffc, dfdi, nd, tau1, tau2)
        else if (ndim.eq.2) then
            elrefc='NON'
            call xjacf2(elref, elrefc, elc, ndim, fpg,&
                        jptint, ifa, cface, ndim, ipgf,&
                        nnop, igeom, jbasec, xg, jac,&
                        ff, r27bid, dfdi, nd, tau1)
        endif
        if (option .ne. 'CALC_K_G_COHE') then
            call xsifl1(angl, basloc, coeff, coeff3, ddlm,&
                        ddls, dfdi, ff, he, idepl,&
                        igthet, ipref, ipres, ithet, jac,&
                        jlst, ka, mu, mult, nd,&
                        ndim, nfh, nnop, nnops, itemps,&
                        nompar, option, presn, singu, xg)
        endif
        if (option .eq. 'CALC_K_G_COHE') then
            call xsifl2(basloc, coeff, coeff3, ddld, ddlm,&
                        ddls, dfdi, ff, idepl, igthet,&
                        ithet, jac, mult, ndim, nnop,&
                        nnos, tau1, tau2, nd, xg)
        endif
900  end do
!     FIN DE BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
!     ----------------------------------------------------------------
    call jedema()
!
end subroutine
