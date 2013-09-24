subroutine xbsir2(elref, contac, ddlc, ddlm, ddls,&
                  igeom, jfisno, jlst, ivectu, singu,&
                  nddl, ndim, nfe, nfh, nfiss,&
                  nno, nnom, nnos, depref, sigref,&
                  nomte)
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/elelin.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/vecini.h"
#include "asterfort/xmprep.h"
#include "asterfort/xmulco.h"
#include "asterfort/xmvco3.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! IN ELREF  : ELEMENT DE REF PARENT
! IN CONTAC : DISCRETISATION, 1 POUR P1P1, 3 POUR P2P1
! IN DDLC   : NB DDL DE CONTACT PAR NOEUD SOMMET
! IN DDLM   : NB DDL PAR NOEUD MILIEU
! IN DDLS   : NB DDL TOT PAR NOEUD SOMMET
! IN GEOM   : ADRESSE POUR COORDONNEES NOEUD PARENT
! IN JFISNO : CONNECTIVITE FISSURE/DDLS HEAVISIDE AU NOEUD
! IN JLST   : ADRESSE LST
! IN/OUT IVECTU : VECTEUR RESIDUS DE REF
! IN SINGU
! IN NDDL   : NB TOTAL DDL ELEMENT
! NDIM      : DIMENSION DU MODELE
! NFE       : NB FONCTION ENRICHISSEMENT CTIP
! NFH       : IDEM HEAVISIDE
! NFISS     : NB FISSURES
! NNO       : NB NOEUD ELEM PARENT
! NNOM      : DONT NB NOEUDS MILIEUX
! NNOS      : DONT NB NOEUDS SOMMETS
! DEPREF    : DEPLACEMENT DE REFERENCE
! SIGREF    : CONTRAINTE DE REFERENCE
! NOMTE     : TYPE D ELEMENT
! -------------------
! CALCUL RESIDU DE REFERENCE ELEMENTS COHESIF MIXTE XFEM
! TERMES D INTERFACE
! -------------------
    integer :: cface(5, 3), contac, ddlc, ddlm, ddls
    integer :: i, iadzi, iazk24, vstnc(1), ibid, ifa, ifiss, igeom, ipgf
    integer :: iret, jaint, jbasec, jcface
    integer :: jfisno, jheafa, jheano, jlonch, jlst, jptint, jtab(2)
    integer :: ivectu, lact(8), singu
    integer :: nbspg, ncompa, ncompb, ncompc, ncomph, ncompp
    integer :: nddl, ndim, nface, nfe, nfh, nfiss, ninter, nlact
    integer :: nno, nnol, nnom, nnos, npgf, nptf, nspfis, pla(27)
    integer :: idfdef, ipoidf, ivff, j, nnof
    real(kind=8) :: depref, ffc(8), ffp(27), jac
    real(kind=8) :: r3bid(3), rr, sigref, vtmp(400)
    logical :: lbid
    character(len=8) :: elc, elref, elrefc, fpg, typma
    character(len=16) :: nomte
!
! --- INITIALISATIONS
!
    do 5 i = 1, 8
        lact(i) = 0
 5  end do
    call vecini(27, 0.d0, ffp)
    call vecini(400, 0.d0, vtmp)
    rr = 0.d0
    ncomph = 0
    nbspg = 0
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
! --- ROUTINE SPECIFIQUE P2P1
!
    call elelin(contac, elref, elrefc, ibid, ibid)
!
! --- ARGUMENTS SUPPLEMENTAIRES NECESSAIRES PAR RAPPORT
! --- AUX ELEMENTS VOLUMIQUES
!
    call jevech('PPINTER', 'L', jptint)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PBASECO', 'L', jbasec)
    call jevech('PLONFA', 'L', jlonch)
    if (nfiss .gt. 1) then
        call jevech('PHEAVNO', 'L', jheano)
        call jevech('PHEAVFA', 'L', jheafa)
        call tecach('OOO', 'PHEAVFA', 'L', iret, nval=2,&
                    itab=jtab)
        ncomph = jtab(2)
    endif
!     DIMENSIONS DES GRANDEURS DANS LA CARTE
    call tecach('OOO', 'PPINTER', 'L', iret, nval=2,&
                itab=jtab)
    ncompp = jtab(2)
    call tecach('OOO', 'PAINTER', 'L', iret, nval=2,&
                itab=jtab)
    ncompa = jtab(2)
    call tecach('OOO', 'PBASECO', 'L', iret, nval=2,&
                itab=jtab)
    ncompb = jtab(2)
    call tecach('OOO', 'PCFACE', 'L', iret, nval=2,&
                itab=jtab)
    ncompc = jtab(2)
!
! --- BOUCLE SUR LES FISSURES
!
    do 90 ifiss = 1, nfiss
!
! --- RECUPERATION DIVERSES DONNEES CONTACT
!
        ninter=zi(jlonch+3*(ifiss-1)-1+1)
        if (ninter .eq. 0) goto 90
!
        fpg = 'FPG2'
! SCHEMA EN DUR POUR LE MOMENT
        if (ndim .eq. 3) then
            elc='TR3'
        else if (ndim.eq.2) then
            if (contac .le. 2) then
                elc='SE2'
            else
                elc='SE3'
            endif
        endif
!
        call elref4(elc, fpg, ibid, nnof, ibid,&
                    npgf, ipoidf, ivff, idfdef, ibid)
        nface=zi(jlonch+3*(ifiss-1)-1+2)
        nptf=zi(jlonch+3*(ifiss-1)-1+3)
        do 11 i = 1, nface
            do 12 j = 1, nptf
                cface(i,j)=zi(jcface-1+nptf*(i-1)+j)
12          continue
11      continue
!
        nspfis = npgf*nface
!
!
! --- RECUP MULTIPLICATEURS ACTIFS ET LEURS INDICES
!
        call xmulco(contac, ddlc, ddlm, jaint, ifiss,&
                    jheano, vstnc, lact, .false., lbid,&
                    ndim, nfe, nfh, nfiss, ninter,&
                    nlact, nno, nnol, nnom, nnos,&
                    pla, typma)
!
! --- BOUCLE SUR LES FACETTES
!
        do 100 ifa = 1, nface
!
! --- BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
!
            do 110 ipgf = 1, npgf
!
! --- PREPARATION DU CALCUL
!
                call xmprep(cface, contac, elref, elrefc, elc,&
                            ffc, ffp, fpg, jaint, jbasec,&
                            jptint, ifa, igeom, ipgf, jac,&
                            jlst, lact, r3bid, ndim, ninter,&
                            nlact, nno, nnos, nptf, ibid,&
                            rr, singu, r3bid, r3bid)
!
! --- CALCUL VECTEURS DE REFERENCE POUR LA LOI D INTERFACE
!
                call xmvco3(sigref, depref, ndim, nno, nnol,&
                            nnos, pla, lact, nfh, ddls,&
                            ddlm, nfiss, ifiss, jheafa, ifa,&
                            ncomph, jfisno, jac, ffc, ffp,&
                            singu, rr, vtmp)
! --- FIN DE BOUCLE SUR LES POINTS DE GAUSS
110          continue
!
! --- FIN DE BOUCLE SUR LES FACETTES
100      continue
! --- FIN BOUCLE SUR LES FISSURES
        nbspg = nbspg + nspfis
        jbasec = jbasec + ncompb
        jptint = jptint + ncompp
        jaint = jaint + ncompa
        jcface = jcface + ncompc
90  end do
!
!-----------------------------------------------------------------------
!     COPIE DES CHAMPS DE SORTIES ET FIN
!-----------------------------------------------------------------------
!
    do 900 i = 1, nddl
        zr(ivectu-1+i)=zr(ivectu-1+i)+vtmp(i)
900  end do
!
end subroutine
