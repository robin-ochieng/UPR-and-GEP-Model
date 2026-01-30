# LRC Model - Landing Page Project Plan

**Project Title:** Premium, Professional & Modern Landing Page for LRC Model  
**Prepared by:** Kenbright AI Development Team  
**Date:** January 30, 2026  
**Version:** 1.0

---

## 1. Executive Summary

### 1.1 Project Overview
This project aims to create a premium, professional, and modern landing page for the **LRC Model** (Liability for Remaining Coverage Model) - an actuarial tool that calculates Unearned Premium Reserves (UPR), Gross/Net Earned Premiums (GEP/NEP), and related insurance metrics. The landing page will serve as the primary entry point for clients, explaining the tool's capabilities, benefits, and outputs.

### 1.2 Business Objectives
- Provide a compelling first impression for clients accessing the tool
- Clearly communicate the value proposition and capabilities of the LRC Model
- Guide users through the tool's features and expected outputs
- Establish trust and professionalism through modern design
- Serve as educational material for new users

### 1.3 Target Audience
- Insurance company actuaries and analysts
- Risk management professionals
- Finance teams requiring premium reserve calculations
- Reinsurance professionals
- Regulatory compliance officers

---

## 2. Current Application Understanding

### 2.1 What the Tool Does

The **LRC Model** is an R Shiny-based actuarial application that performs:

| Feature | Description |
|---------|-------------|
| **Data Upload & Validation** | Accepts Excel/CSV files with policy data and validates required columns |
| **UPR Calculation** | Calculates Unearned Premium Reserves for direct business and reinsurance |
| **DAC Calculation** | Computes Deferred Acquisition Costs |
| **LRC Calculation** | Liability for Remaining Coverage = Gross UPR - DAC - Premium Receivables + Bad Debt |
| **ARC Calculation** | Asset for Remaining Coverage = RI Gross UPR - RI DAC - Premium Receivables |
| **Net LRC** | Net Liability = LRC - ARC |
| **GEP Analysis** | Gross Earned Premiums by IRA Class (Monthly/Quarterly) |
| **NEP Analysis** | Net Earned Premiums by IRA Class (Monthly/Quarterly) |

### 2.2 Required Data Inputs
- **BegDate** - Policy start date
- **EndDate** - Policy end date
- **AuthDate** - Policy underwriting date
- **IRA CLASS** - Insurance class of business
- **Gross Premium** - Gross premium amount
- **Net Premium** - Net premium amount
- **Commission** - Commission amount
- **RI_Premium** - Reinsurance premium
- **RI_Commission** - Reinsurance commission

### 2.3 Key Outputs
1. **UPR Summaries** - Value boxes showing Gross UPR, DAC, RI UPR, RI DAC
2. **Class-wise Tables** - Breakdowns by IRA Insurance Class
3. **Visualizations** - Bar charts for UPR and DAC distributions
4. **LRC/ARC/Net LRC Tables** - Editable tables with calculated liabilities
5. **GEP/NEP Reports** - Time-based earned premium analysis with CSV downloads

---

## 3. Project Phases

### Phase 1: Discovery & Design Planning (Week 1)

#### 3.1.1 Objectives
- Finalize landing page requirements and content structure
- Gather brand assets and design guidelines
- Create wireframes and design mockups

#### 3.1.2 Deliverables
| Deliverable | Description | Owner |
|-------------|-------------|-------|
| Content Outline | Detailed outline of all landing page sections | Content Team |
| Wireframes | Low-fidelity layouts for desktop and mobile | UI/UX Designer |
| Brand Guidelines | Color palette, typography, imagery style | Design Team |
| Stakeholder Sign-off | Approval on wireframes and content structure | Project Manager |

#### 3.1.3 Tasks
- [ ] Conduct stakeholder interviews for content requirements
- [ ] Research competitor/industry landing pages for inspiration
- [ ] Define key messaging and value propositions
- [ ] Create low-fidelity wireframes (desktop + mobile)
- [ ] Select hero imagery and iconography style
- [ ] Review and approve design direction

---

### Phase 2: UI/UX Design (Week 2)

#### 3.2.1 Objectives
- Create high-fidelity design mockups
- Establish responsive design patterns
- Design all visual elements and animations

#### 3.2.2 Deliverables
| Deliverable | Description | Owner |
|-------------|-------------|-------|
| High-Fidelity Mockups | Pixel-perfect designs for all sections | UI Designer |
| Design System | Reusable components library | UI Designer |
| Animation Specs | Micro-interaction and scroll animation specs | UI/UX Designer |
| Mobile Designs | Responsive layouts for tablet and mobile | UI Designer |

#### 3.2.3 Proposed Landing Page Sections

```
┌─────────────────────────────────────────────────────────────┐
│  1. HERO SECTION                                            │
│     - Headline: "Precision Actuarial Analytics"             │
│     - Subheadline: Value proposition                        │
│     - CTA: "Launch Application" / "Learn More"              │
│     - Hero image/animation                                  │
├─────────────────────────────────────────────────────────────┤
│  2. KEY FEATURES                                            │
│     - UPR Calculation                                       │
│     - GEP/NEP Analysis                                      │
│     - LRC/ARC Calculation                                   │
│     - Data Export                                           │
├─────────────────────────────────────────────────────────────┤
│  3. HOW IT WORKS                                            │
│     - Step 1: Upload Data                                   │
│     - Step 2: Configure Parameters                          │
│     - Step 3: Calculate & Analyze                           │
│     - Step 4: Export Reports                                │
├─────────────────────────────────────────────────────────────┤
│  4. OUTPUTS SHOWCASE                                        │
│     - Interactive preview of dashboards                     │
│     - Sample visualizations                                 │
│     - Table examples                                        │
├─────────────────────────────────────────────────────────────┤
│  5. TECHNICAL SPECIFICATIONS                                │
│     - Required data columns                                 │
│     - Supported file formats                                │
│     - Calculation methodologies                             │
├─────────────────────────────────────────────────────────────┤
│  6. TESTIMONIALS / TRUST INDICATORS                         │
│     - Client logos (if applicable)                          │
│     - Industry compliance badges                            │
├─────────────────────────────────────────────────────────────┤
│  7. CALL TO ACTION                                          │
│     - "Get Started" button                                  │
│     - Contact/Support information                           │
├─────────────────────────────────────────────────────────────┤
│  8. FOOTER                                                  │
│     - Kenbright branding                                    │
│     - Quick links                                           │
│     - Copyright                                             │
└─────────────────────────────────────────────────────────────┘
```

#### 3.2.4 Design Specifications

**Color Palette (Based on existing theme):**
| Color | Hex Code | Usage |
|-------|----------|-------|
| Primary Blue | #0137A6 | CTAs, Headers, Accents |
| Secondary Blue | #6EA8FE | Hover states, Links |
| Background | #F3F8FF | Page background |
| Dark Text | #102A56 | Body text, Headings |
| White | #FFFFFF | Cards, Containers |

**Typography:**
- **Primary Font:** Mulish (Google Fonts) - already in use
- **Headings:** Bold, sizes 24-48px
- **Body:** Regular, 16-18px
- **Captions:** Light, 14px

---

### Phase 3: Development - Structure & Components (Week 3)

#### 3.3.1 Objectives
- Set up landing page architecture
- Develop reusable UI components
- Implement responsive layout

#### 3.3.2 Technical Approach Options

| Option | Technology | Pros | Cons |
|--------|------------|------|------|
| **Option A** | Integrate into Shiny App | Single deployment, consistent | Limited design flexibility |
| **Option B** | Separate HTML/CSS/JS | Full design control, modern | Separate hosting needed |
| **Option C** | Shiny with Custom Tab | Easy integration, Shiny-native | Some design limitations |

**Recommended:** Option C - Create a "Welcome" or "Home" tab within the existing Shiny app that serves as the landing page, with custom HTML/CSS for premium design.

#### 3.3.3 Deliverables
| Deliverable | Description | Owner |
|-------------|-------------|-------|
| Landing Page Module | `landingPageModule.R` with UI and Server | Developer |
| Custom CSS | `landing_page.css` with all styles | Developer |
| Asset Files | Optimized images, icons, animations | Designer/Developer |
| Responsive Grid | Mobile-first responsive layout | Developer |

#### 3.3.4 Tasks
- [ ] Create `modules/landingPageModule.R`
- [ ] Create `www/css/landing_page.css`
- [ ] Add landing page images to `www/images/`
- [ ] Implement hero section with animations
- [ ] Build feature cards component
- [ ] Create "How It Works" timeline
- [ ] Develop outputs showcase section

---

### Phase 4: Development - Content & Interactions (Week 4)

#### 3.4.1 Objectives
- Populate all content sections
- Implement animations and micro-interactions
- Add navigation and scroll effects

#### 3.4.2 Deliverables
| Deliverable | Description | Owner |
|-------------|-------------|-------|
| Animated Hero | CSS/JS animations for hero section | Developer |
| Interactive Features | Hover effects, tooltips | Developer |
| Scroll Animations | Fade-in, slide-up effects | Developer |
| Navigation | Smooth scroll to sections | Developer |

#### 3.4.3 Content Requirements

**Hero Section Content:**
```
Headline: "LRC Model"
Subheadline: "Advanced Actuarial Analytics for Insurance Premium Management"
Description: "Calculate Unearned Premium Reserves, Earned Premiums, and 
              Liability for Remaining Coverage with precision and ease."
CTA Primary: "Launch Application →"
CTA Secondary: "View Documentation"
```

**Feature Cards Content:**
1. **Unearned Premium Reserves**
   - Icon: calculator
   - Description: Calculate Gross UPR and Deferred Acquisition Costs across all insurance classes

2. **Earned Premium Analysis**
   - Icon: chart-line
   - Description: Monthly and quarterly GEP/NEP summaries with customizable time periods

3. **LRC/ARC Calculation**
   - Icon: balance-scale
   - Description: Compute Liability and Asset for Remaining Coverage with editable parameters

4. **Export & Reporting**
   - Icon: download
   - Description: Download comprehensive CSV reports for all calculations

**How It Works Steps:**
1. **Upload Your Data** - Import Excel or CSV files with policy premium data
2. **Set Parameters** - Configure valuation date and policy year thresholds
3. **Calculate** - Generate UPR, GEP, LRC, and other actuarial metrics
4. **Analyze & Export** - Review visualizations and download reports

---

### Phase 5: Integration & Testing (Week 5)

#### 3.5.1 Objectives
- Integrate landing page into main application
- Perform cross-browser and device testing
- Optimize performance

#### 3.5.2 Deliverables
| Deliverable | Description | Owner |
|-------------|-------------|-------|
| Integrated App | Landing page in main `app.R` | Developer |
| Test Report | Cross-browser/device test results | QA |
| Performance Report | Load time and optimization metrics | Developer |

#### 3.5.3 Testing Checklist
- [ ] Desktop browsers: Chrome, Firefox, Edge, Safari
- [ ] Mobile browsers: iOS Safari, Android Chrome
- [ ] Tablet responsiveness
- [ ] Animation performance (60fps target)
- [ ] Page load time (< 3 seconds target)
- [ ] Accessibility (WCAG 2.1 AA compliance)
- [ ] Link validation
- [ ] CTA functionality

---

### Phase 6: Refinement & Launch (Week 6)

#### 3.6.1 Objectives
- Address feedback and bug fixes
- Final stakeholder review
- Deploy to production

#### 3.6.2 Deliverables
| Deliverable | Description | Owner |
|-------------|-------------|-------|
| Final Landing Page | Production-ready landing page | Developer |
| Documentation | User guide updates | Content Team |
| Deployment | Production deployment | DevOps |

#### 3.6.3 Tasks
- [ ] Incorporate stakeholder feedback
- [ ] Final design polish
- [ ] Performance optimization
- [ ] Security review
- [ ] Production deployment
- [ ] Post-launch monitoring

---

## 4. Technical Implementation Details

### 4.1 File Structure After Implementation

```
UPR-and-GEP-Model/
├── app.R                          # Main app (updated with landing page)
├── modules/
│   ├── landingPageModule.R        # NEW: Landing page module
│   ├── dataOverviewModule.R
│   ├── uprSummaries.R
│   ├── gepResultsModule.R
│   └── helperFunctions.R
├── www/
│   ├── css/
│   │   ├── custom_styles.css
│   │   └── landing_page.css       # NEW: Landing page styles
│   ├── images/
│   │   ├── kenbright.png
│   │   ├── hero-bg.png            # NEW: Hero background
│   │   ├── feature-icons/         # NEW: Feature icons
│   │   └── screenshots/           # NEW: App screenshots
│   └── js/
│       └── landing_animations.js  # NEW: Optional JS animations
└── docs/
    ├── LANDING_PAGE_PROJECT_PLAN.md  # This document
    └── USER_GUIDE.md                  # NEW: User documentation
```

### 4.2 Code Integration Approach

**app.R modifications:**
```r
# Add landing page as first tab in sidebar
bs4SidebarMenu(
  bs4SidebarMenuItem("Home", tabName = "landing", icon = icon("home")),
  bs4SidebarMenuItem("Data Overview", tabName = "dataOverview", icon = icon("table")),
  # ... existing menu items
)

# Add landing page tab item
bs4TabItems(
  bs4TabItem(tabName = "landing",
    landingPageUI("landing_page")
  ),
  # ... existing tab items
)
```

### 4.3 CSS Framework Approach

Leverage existing bs4Dash styling while adding custom enhancements:
- CSS Grid for section layouts
- CSS Flexbox for component alignment
- CSS Variables for theme consistency
- CSS Animations for micro-interactions
- Media queries for responsiveness

---

## 5. Success Metrics

| Metric | Target | Measurement Method |
|--------|--------|-------------------|
| Page Load Time | < 3 seconds | Browser DevTools |
| Mobile Responsiveness | 100% functional | Device testing |
| User Engagement | > 60% proceed to app | Analytics (optional) |
| Accessibility Score | > 90% | Lighthouse audit |
| Design Approval | Stakeholder sign-off | Review meeting |

---

## 6. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Design delays | Medium | High | Early stakeholder involvement |
| Integration issues | Low | Medium | Incremental development |
| Performance impact | Low | High | Lazy loading, optimization |
| Content not ready | Medium | Medium | Use placeholder content |

---

## 7. Timeline Summary

| Phase | Duration | Start | End |
|-------|----------|-------|-----|
| Phase 1: Discovery & Design Planning | 1 week | Week 1 | Week 1 |
| Phase 2: UI/UX Design | 1 week | Week 2 | Week 2 |
| Phase 3: Development - Structure | 1 week | Week 3 | Week 3 |
| Phase 4: Development - Content | 1 week | Week 4 | Week 4 |
| Phase 5: Integration & Testing | 1 week | Week 5 | Week 5 |
| Phase 6: Refinement & Launch | 1 week | Week 6 | Week 6 |

**Total Project Duration:** 6 weeks

---

## 8. Resource Requirements

| Role | Allocation | Responsibilities |
|------|------------|------------------|
| Project Manager | 20% | Coordination, stakeholder management |
| UI/UX Designer | 50% | Wireframes, mockups, design system |
| Frontend Developer | 100% | Implementation, integration |
| QA Engineer | 25% | Testing, accessibility |
| Content Writer | 25% | Copy, documentation |

---

## 9. Appendix

### A. Reference Landing Pages for Inspiration
- Modern SaaS landing pages
- Financial/Insurance software products
- Data analytics platforms

### B. Brand Assets Needed
- High-resolution Kenbright logo
- Brand color specifications
- Typography licenses (Mulish is free)
- Stock images or custom illustrations

### C. Content Checklist
- [ ] Hero headline and subheadline
- [ ] Feature descriptions (4-6 features)
- [ ] How it works steps (4 steps)
- [ ] Technical specifications content
- [ ] Call-to-action copy
- [ ] Footer content

---

**Document Status:** Draft  
**Next Review Date:** [TBD]  
**Approvers:** [TBD]

---

*Developed by Kenbright AI © 2026*
