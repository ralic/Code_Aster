subroutine te0533(option, nomte)
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
    include 'asterfort/matini.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xdocon.h'
    include 'asterfort/xmcont.h'
    include 'asterfort/xmfrot.h'
    include 'asterfort/xmprep.h'
    include 'asterfort/xmulco.h'
    include 'asterfort/xteddl.h'
    include 'asterfort/xteini.h'
    character(len=16) :: option, nomte
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!         CALCUL DES MATRICES DE CONTACT FROTTEMENT POUR X-FEM
!                       (METHODE CONTINUE)
!
!
!  OPTION : 'RIGI_CONT' (CALCUL DES MATRICES DE CONTACT)
!  OPTION : 'RIGI_FROT' (CALCUL DES MATRICES DE FROTTEMENT)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!......................................................................
!
    integer :: i, j, ij, ifa, ipgf, isspg
    integer :: jindco, jdonco, jlsn, ipoids, ivf, idfde, jgano, igeom
    integer :: idepm, idepd, imatt, jlst, jptint, jaint, jcface, jlonch
    integer :: ivff, iadzi, iazk24, ibid, jbasec, jseuil
    integer :: ndim, nfh, ddlc, ddls, nddl, nno, nnos, nnom, nnof, ddlm
    integer :: npg, npgf, fac(6, 4), nbf, algocr, algofr, vstnc(32)
    integer :: indco, ninter, nface, cface(5, 3), ibid2(12, 3)
    integer :: nfe, singu, jstno, nvit, jcoheo, ncompv
    integer :: nnol, pla(27), lact(8), nlact, nptf
    integer :: contac, nfiss, jfisno, jmate, jcohes, nbspg, nspfis
    real(kind=8) :: ffp(27), ffc(8), coefcp, coefcr, coeffp
    real(kind=8) :: mmat(216, 216), jac, mu, coeffr, rela
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: nd(3), seuil, cohes(3)
    real(kind=8) :: rr, rbid, coheo(3)
    integer :: jheano, ifiss, jstnc, jheafa, ncomph
    integer :: jtab(2), iret, ncompd, ncompp, ncompa, ncompb, ncompc
    logical :: matsym, lelim
    character(len=8) :: elref, elrefc, typma
    character(len=8) :: elc, fpg
!......................................................................
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 6 i = 1, 8
        lact(i) = 0
 6  end do
    call vecini(27, 0.d0, ffp)
    call vecini(3, 0.d0, tau2)
    lelim = .false.
    nbspg=0
! INTIALISATION JMATE POUR DETECTER EVENTUELLES ERREURS JEVEUX
    jmate=1
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
!     INITIALISATION DE LA MATRICE DE TRAVAIL
    call matini(216, 216, 0.d0, mmat)
!
! --- ROUTINE SPECIFIQUE P2P1, A CONSERVER
!
    call elelin(contac, elref, elrefc, ibid, ibid)
!
! --- RECUPERATION DES ENTREES / SORTIE
!
    call jevech('PGEOMER', 'L', igeom)
!     DEPMOI
    call jevech('PDEPL_M', 'L', idepm)
!     DEPDEL
    call jevech('PDEPL_P', 'L', idepd)
    call jevech('PINDCOI', 'L', jindco)
    call jevech('PDONCO', 'L', jdonco)
    call jevech('PSEUIL', 'L', jseuil)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTER', 'L', jptint)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASECO', 'L', jbasec)
    if (nfiss .gt. 1) then
        call jevech('PFISNO', 'L', jfisno)
        call jevech('PHEAVNO', 'L', jheano)
        call jevech('PHEAVFA', 'L', jheafa)
        call tecach('OOO', 'PHEAVFA', 'L', 2, jtab,&
                    iret)
        ncomph = jtab(2)
    endif
!     NB COMPOSANTES DES MODES LOCAUX
!     ASSOCIES AUX CHAMPS DANS LE CATALOGUE
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
        if (algocr .eq. 3) then
            call jevech('PMATERC', 'L', jmate)
            call jevech('PCOHES', 'L', jcohes)
            call tecach('OOO', 'PCOHES', 'L', 2, jtab,&
                        iret)
            ncompv = jtab(2)
            if (option .eq. 'RIGI_CONT') then
                call jevech('PCOHESO', 'E', jcoheo)
            endif
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
! --- CALCUL DES MATRICES DE CONTACT
!     ..............................
!
                if (option .eq. 'RIGI_CONT') then
!
                    call xmcont(algocr, coefcr, coefcp, cohes, coheo,&
                                ddlm, ddls, ffc, ffp, idepd,&
                                idepm, ifa, ifiss, jmate, indco,&
                                ipgf, jac, jfisno, jheafa, mmat,&
                                lact, ncomph, nd, nddl, ndim,&
                                nfh, nfiss, nno, nnol, nnos,&
                                nvit, pla, rela, rr, singu,&
                                tau1, tau2)
!
! --- ACTUALISATION VARIABLE INTERNE
!
                    if (algocr .eq. 3) then
                        do 3 i = 1, ncompv
                            zr(jcoheo+ncompv*(nbspg+isspg-1)-1+i) =&
                            coheo(i)
 3                      continue
                    endif
!
                    elseif (option.eq.'RIGI_FROT' .and.rela.ne.3.d0.and.&
                    rela.ne.4.d0) then
!
                    call xmfrot(algofr, coeffr, coeffp, ddlm, ddls,&
                                ffc, ffp, idepd, idepm, indco,&
                                jac, lact, mmat, mu, nd,&
                                ndim, nfh, nfiss, nno, nnol,&
                                nnos, nvit, pla, rr, seuil,&
                                singu, tau1, tau2)
!
                else
                    call assert(rela.eq.3.d0.or.rela.eq.4.d0)
                endif
! --- FIN DE BOUCLE SUR LES POINTS DE GAUSS
110          continue
!
! --- FIN DE BOUCLE SUR LES FACETTES
100      continue
! --- FIN BOUCLE SUR LES FISSURES : DECALAGE D INDICES
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
    if (algocr .eq. 2 .or. algofr .eq. 2 .or. (algocr.eq.3.and.option.eq.'RIGI_FROT')) then
! --- RECUPERATION DE LA MATRICE 'OUT' NON SYMETRIQUE
        matsym=.false.
        call jevech('PMATUNS', 'E', imatt)
        do 201 j = 1, nddl
            do 211 i = 1, nddl
                ij = j+nddl*(i-1)
                zr(imatt+ij-1) = mmat(i,j)
211          continue
201      continue
    else
! --- RECUPERATION DE LA MATRICE 'OUT' SYMETRIQUE
        matsym=.true.
        call jevech('PMATUUR', 'E', imatt)
        do 200 j = 1, nddl
            do 210 i = 1, j
                ij = (j-1)*j/2 + i
                zr(imatt+ij-1) = mmat(i,j)
210          continue
200      continue
    endif
! --- SUPPRESSION DES DDLS DE DEPLACEMENT SEULEMENT POUR LES XHTC
    if (nfh .ne. 0) then
        call jevech('PSTANO', 'L', jstno)
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false., matsym,&
                    option, nomte, zr(imatt), rbid, ddlm,&
                    nfiss, jfisno)
    endif
! --- SUPPRESSION DES DDLS DE CONTACT
    if (lelim) then
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, vstnc, .true., matsym,&
                    option, nomte, zr(imatt), rbid, ddlm,&
                    nfiss, jfisno)
    endif
!
    call jedema()
end subroutine
