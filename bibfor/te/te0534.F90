subroutine te0534(option, nomte)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/confac.h'
    include 'asterfort/elelin.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/teattr.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xdocon.h'
    include 'asterfort/xmprep.h'
    include 'asterfort/xmulco.h'
    include 'asterfort/xteddl.h'
    include 'asterfort/xteini.h'
    include 'asterfort/xvcont.h'
    include 'asterfort/xvfrot.h'
    include 'asterfort/xxlagm.h'
    character(len=16) :: option, nomte
!
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
! person_in_charge: samuel.geniaut at edf.fr
! TOLE CRP_20
!
!.......................................................................
!
!               CALCUL DES SECONDS MEMBRES DE CONTACT FROTTEMENT
!                   POUR X-FEM  (METHODE CONTINUE)
!
!
!  OPTION : 'CHAR_MECA_CONT' (CALCUL DU SECOND MEMBRE DE CONTACT)
!  OPTION : 'CHAR_MECA_FROT' (CALCUL DU SECOND MEMBRE DE FROTTEMENT)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!.......................................................................
!
!
    integer :: i, j, ifa, ipgf, isspg
    integer :: jindco, jdonco, jlst, ipoids, ivf, idfde, jgano, igeom
    integer :: idepm, idepl, jptint, jaint, jcface, jlonch
    integer :: ivff, iadzi, iazk24, ibid, ivect, jbasec
    integer :: ndim, nfh, ddlc, ddls, nddl, nno, nnos, nnom, nnof, ddlm
    integer :: npg, npgf, fac(6, 4), nbf, jseuil
    integer :: indco, ninter, nface, cface(5, 3), ibid2(12, 3)
    integer :: nfe, singu, jstno, nvit, algocr, algofr, nvec
    integer :: nnol, pla(27), lact(8), nlact, jcohes
    integer :: jmate, nfiss, jfisno, contac, nbspg, nspfis
    integer :: jheano, ifiss, ncomph, ncompv, vstnc(32)
    integer :: jtab(2), iret, ncompd, ncompp, ncompa, ncompb, ncompc
    integer :: jheafa, nptf
    real(kind=8) :: vtmp(400), reac, reac12(3), jac
    real(kind=8) :: nd(3), ffp(27), ffc(8), seuil, coefcp, coefcr, coeffp
    real(kind=8) :: mu, tau1(3), tau2(3), coeffr
    real(kind=8) :: rr, rbid, cohes(3), rela
    logical :: lbid, lelim
    character(len=8) :: elref, typma, elrefc
    character(len=8) :: elc, fpg
    character(len=16) :: enr
!
!.......................................................................
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 5 i = 1, 8
        lact(i) = 0
 5  end do
    call vecini(27, 0.d0, ffp)
    rr = 0.d0
    ncomph = 0
    lelim = .false.
    nbspg = 0
    call vecini(3, 0.d0, tau2)
    call elref1(elref)
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                contac)
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
!
    if (ndim .eq. 3) then
        call confac(typma, ibid2, ibid, fac, nbf)
    endif
!
    do 40 j = 1, nddl
        vtmp(j)=0.d0
40  end do
!
! --- ROUTINE SPECIFIQUE P2P1
!
    call elelin(contac, elref, elrefc, ibid, ibid)
!
! --- RECUPERATION DES ENTRÉES / SORTIE
!
    call jevech('PGEOMER', 'L', igeom)
!     DEPLACEMENT A L'EQUILIBRE PRECEDENT  (DEPMOI)       : 'PDEPL_M'
    call jevech('PDEPL_M', 'L', idepm)
!     INCREMENT DE DEP DEPUIS L'EQUILIBRE PRECEDENT (DEPDEL) :'PDEPL_P'
    call jevech('PDEPL_P', 'L', idepl)
    call jevech('PINDCOI', 'L', jindco)
    call jevech('PDONCO', 'L', jdonco)
    call tecach('OOO', 'PDONCO', 'L', 2, jtab,&
                ibid)
    ncompd = jtab(2)
    call jevech('PSEUIL', 'L', jseuil)
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTER', 'L', jptint)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASECO', 'L', jbasec)
    call jevech('PVECTUR', 'E', ivect)
    if (nfiss .gt. 1) then
        call jevech('PFISNO', 'L', jfisno)
        call jevech('PHEAVNO', 'L', jheano)
        call jevech('PHEAVFA', 'L', jheafa)
        call tecach('OOO', 'PHEAVFA', 'L', 2, jtab,&
                    iret)
        ncomph = jtab(2)
    endif
!     DIMENSSION DES GRANDEURS DANS LA CARTE
    call tecach('OOO', 'PDONCO', 'L', 2, jtab,&
                iret)
    ncompd = jtab(2)
    call tecach('OOO', 'PPINTER', 'L', 2, jtab,&
                iret)
    ncompp = jtab(2)
    call tecach('OOO', 'PAINTER', 'L', 2, jtab,&
                iret)
    ncompa = jtab(2)
    call tecach('OOO', 'PBASECO', 'L', 2, jtab,&
                iret)
    ncompb = jtab(2)
    call tecach('OOO', 'PCFACE', 'L', 2, jtab,&
                iret)
    ncompc = jtab(2)
!
!     STATUT POUR L'ÉLIMINATION DES DDLS DE CONTACT
    do 30 i = 1, max(1, nfh)*nnos
        vstnc(i) = 1
30  end do
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
!
!
! --- BOUCLE SUR LES FISSURES
!
    do 90 ifiss = 1, nfiss
!
! --- RECUPERATION DIVERSES DONNEES CONTACT
!
        call xdocon(algocr, algofr, cface, contac, coefcp,&
                    coeffp, coefcr, coeffr, elc, fpg,&
                    ifiss, ivff, jcface, jdonco, jlonch,&
                    mu, nspfis, ncompd, ndim, nface,&
                    ninter, nnof, nomte, npgf, nptf,&
                    rela)
        if (ninter .eq. 0) goto 91
!
! --- RECUPERATION MATERIAU ET VARIABLES INTERNES COHESIF
!
        jmate=1
        if (algocr .eq. 3) then
            call jevech('PMATERC', 'L', jmate)
            call jevech('PCOHES', 'L', jcohes)
            call tecach('OOO', 'PCOHES', 'L', 2, jtab,&
                        iret)
            ncompv = jtab(2)
        endif
!
! --- RECUP MULTIPLICATEURS ACTIFS ET LEURS INDICES
!
        call xmulco(contac, ddlc, ddlm, jaint, ifiss,&
                    jheano, vstnc, lact, .true., lelim,&
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
! --- RECUPERATION DES STATUTS POUR LE POINT DE GAUSS
!
                isspg = npgf*(ifa-1)+ipgf
                indco = zi(jindco-1+nbspg+isspg)
                if (algofr .ne. 0) seuil = zr(jseuil-1+nbspg+isspg)
                if (algocr .eq. 3) then
                    do 2 i = 1, ncompv
                        cohes(i) = zr(jcohes+ncompv*(nbspg+isspg-1)-1+ i)
 2                  continue
                endif
!
! --- PREPARATION DU CALCUL
!
                call xmprep(cface, contac, elref, elrefc, elc,&
                            ffc, ffp, fpg, jaint, jbasec,&
                            jptint, ifa, igeom, ipgf, jac,&
                            jlst, lact, nd, ndim, ninter,&
                            nlact, nno, nnos, nptf, nvit,&
                            rr, singu, tau1, tau2)
!
! --- CALCUL REACTION DE CONTACT ET DE FROTTEMENT
!
                nvec = 2
                call xxlagm(ffc, idepl, idepm, lact, ndim,&
                            nnol, pla, reac, reac12, tau1,&
                            tau2, nvec)
!
! --- CALCUL DES SECONDS MEMBRES DE CONTACT
!     .....................................
!
                if (option .eq. 'CHAR_MECA_CONT') then
                    call xvcont(algocr, cohes, coefcp, coefcr, ddlm,&
                                ddls, ffc, ffp, idepl, idepm,&
                                ifa, ifiss, zi( jmate), indco, ipgf,&
                                jac, jfisno, jheafa, lact, ncomph,&
                                nd, nddl, ndim, nfh, nfiss,&
                                nno, nnol, nnos, nvit, pla,&
                                rela, reac, rr, singu, tau1,&
                                tau2, vtmp)
!
! --- CALCUL DES SECONDS MEMBRES DE FROTTEMENT
!     ........................................
!
                    elseif (option.eq.'CHAR_MECA_FROT' .and.rela.ne.3.d0&
                .and.rela.ne.4.d0) then
                    call xvfrot(algofr, coeffp, coeffr, ddlm, ddls,&
                                ffc, ffp, idepl, idepm, ifa,&
                                ifiss, indco, jac, jfisno, jheafa,&
                                lact, mu, ncomph, nd, nddl,&
                                ndim, nfh, nfiss, nno, nnol,&
                                nnos, nvit, pla, reac12, rr,&
                                seuil, singu, tau1, tau2, vtmp)
!
                else
                    call assert(option .eq. 'CHAR_MECA_FROT' .or. option .eq. 'CHAR_MECA_CONT')
                endif
!
! --- FIN DE BOUCLE SUR LES POINTS DE GAUSS
110          continue
!
! --- FIN DE BOUCLE SUR LES FACETTES
100      continue
! --- FIN BOUCLE SUR LES FISSURES
        nbspg = nbspg + nspfis
91      continue
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
        zr(ivect-1+i)=vtmp(i)
900  end do
!     SUPPRESSION DES DDLS DE DEPLACEMENT SEULEMENT POUR LES XHTC
    if (nfh .ne. 0) then
        call jevech('PSTANO', 'L', jstno)
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false., lbid,&
                    option, nomte, rbid, zr(ivect), ddlm,&
                    nfiss, jfisno)
    endif
!     SUPPRESSION DES DDLS DE CONTACT
    if (lelim) then
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, vstnc, .true., .true.,&
                    option, nomte, rbid, zr(ivect), ddlm,&
                    nfiss, jfisno)
    endif
!
    call jedema()
end subroutine
